// src/App.jsx — Root component

import React, { useState, useEffect } from 'react';
import { createSession, getStoredSessionId, clearSession, getSessionState, apiPost } from './api/session';

import DiabloWorkflowPanel from './components/DiabloWorkflowPanel';
import PipelineNav         from './components/PipelineNav';
import LandingPage         from './components/LandingPage';
import ErrorAlert          from './components/ErrorAlert';
import UploadStep          from './components/UploadStep';
import ValidationStep      from './components/ValidationStep';
import ComparisonStep      from './components/ComparisonStep';
import PreprocessingStep   from './components/PreprocessingStep';
import DiabloStep          from './components/DiabloStep';
import DiabloResultsStep   from './components/DiabloResultsStep';
import DiabloExportStep    from './components/DiabloExportStep';
import CorrelationStep     from './components/CorrelationStep';
import EnrichmentStep      from './components/EnrichmentStep';
import StatsStep           from './components/StatsStep';
import ExportStep          from './components/ExportStep';

// ---------------------------------------------------------------------------
// DIABLO_STEPS — the complete set of step ids owned by the DIABLO module.
// Any step id in this set renders inside the DIABLO layout (left panel + main).
// Generic/shared steps (visualise, export) are NOT in this set.
// ---------------------------------------------------------------------------
const DIABLO_STEPS = new Set([
  'upload',
  'validate',
  'comparison',
  'preprocess',
  'analyse',         // configure + run  (DiabloStep)
  'diablo_results',  // view results     (DiabloResultsStep)
  'diablo_export',   // export           (DiabloExportStep)
]);

// Pipeline state keys → step ids (used to derive active step on session restore)
// NOTE: 'diablo' completion is intentionally NOT mapped to 'diablo_results' here.
// The results step is only reached by completing a run in the current session.
// Restoring a session always lands on 'analyse' (Configure DIABLO) at most,
// so the user must explicitly run the model again.
const STEP_KEYS = ['upload', 'validate', 'comparison', 'preprocess'];
const STEP_IDS  = ['upload', 'validate', 'comparison', 'preprocess', 'analyse'];

function deriveActiveStepId(pipelineState) {
  if (!pipelineState) return 'upload';
  const steps = pipelineState.steps || pipelineState;
  let lastIdx = 0;
  STEP_KEYS.forEach((key, i) => {
    if (steps[key] === 'complete') lastIdx = i + 1;
  });
  const stepId = STEP_IDS[Math.min(lastIdx, STEP_IDS.length - 1)];
  console.log('[DIABLO routing] deriveActiveStepId →', stepId,
    '| upload:', steps.upload, '| validate:', steps.validate,
    '| comparison:', steps.comparison, '| preprocess:', steps.preprocess,
    '| diablo:', steps.diablo);
  return stepId;
}

export default function App() {
  const [sessionId,       setSessionId]       = useState(null);
  const [pipelineState,   setPipelineState]   = useState(null);
  const [activeStep,      setActiveStep]      = useState('upload');
  const [diabloSubStep,   setDiabloSubStep]   = useState('configure');
  const [fatalError,      setFatalError]      = useState(null);
  const [globalError,     setGlobalError]     = useState(null);
  const [datasets,        setDatasets]        = useState([]);
  const [metadataColumns, setMetadataColumns] = useState([]);

  useEffect(() => {
    const stored = getStoredSessionId();
    if (stored) {
      getSessionState(stored)
        .then(state => {
          setSessionId(stored);
          setPipelineState(state);
          setActiveStep(deriveActiveStepId(state));
          if (state.datasets)         setDatasets(state.datasets);
          if (state.metadata_columns) setMetadataColumns(state.metadata_columns);
        })
        .catch(async () => {
          clearSession();
          try { const sid = await createSession(); setSessionId(sid); } catch { /* ignore */ }
          setActiveStep('landing');
        });
    } else {
      setActiveStep('landing');
    }
  }, []);

  async function handleStartPipeline() {
    try {
      const sid = await createSession();
      setSessionId(sid);
    } catch {
      setGlobalError({ error_code: 'SESSION_CREATE_FAILED', detail: 'Could not create a session. Is the backend running?', recoverable: false });
      return;
    }
    setActiveStep('upload');
  }

  async function handleModuleClick(mod) {
    if (sessionId && pipelineState) {
      setActiveStep(mod.navTarget);
    } else {
      try {
        const sid = await createSession();
        setSessionId(sid);
      } catch {
        setGlobalError({ error_code: 'SESSION_CREATE_FAILED', detail: 'Could not create a session. Is the backend running?', recoverable: false });
        return;
      }
      setActiveStep('upload');
    }
  }

  async function handleUploadComplete(filesReceived) {
    const datasetNames = (filesReceived || [])
      .filter(f => !f.toLowerCase().includes('metadata'))
      .map(f => f.replace(/\.csv$/i, ''));
    if (datasetNames.length > 0) setDatasets(datasetNames);
    const state = await getSessionState(sessionId).catch(() => null);
    if (state) setPipelineState(state);
    setActiveStep('validate');
  }

  async function handleStepComplete(nextStepId) {
    if (!sessionId) return;
    try {
      const state = await getSessionState(sessionId);
      setPipelineState(state);
      if (state.datasets)         setDatasets(state.datasets);
      if (state.metadata_columns) setMetadataColumns(state.metadata_columns);
      if (nextStepId !== undefined) setActiveStep(nextStepId);
    } catch { /* non-fatal */ }
  }

  function handleRestartSession() {
    clearSession();
    setSessionId(null);
    setPipelineState(null);
    setActiveStep('landing');
    setFatalError(null);
    setGlobalError(null);
    setDatasets([]);
    setMetadataColumns([]);
    setDiabloSubStep('configure');
  }

  // Reset DIABLO state without clearing the whole session.
  // Called from DiabloStep "Reset DIABLO Run" button and from "Back to Configure"
  // on the results page when the user wants to change parameters.
  async function handleDiabloReset() {
    if (!sessionId) return;
    console.log('[DIABLO routing] handleDiabloReset — clearing DIABLO state');
    try {
      await apiPost(`/${sessionId}/analysis/diablo/reset`, {});
    } catch { /* non-fatal — state may already be clear */ }
    // Refresh pipeline state so the panel reflects the reset
    try {
      const state = await getSessionState(sessionId);
      setPipelineState(state);
      console.log('[DIABLO routing] pipeline state after reset — diablo:', state?.steps?.diablo);
    } catch { /* non-fatal */ }
    setDiabloSubStep('configure');
    setActiveStep('analyse');
  }

  // Panel step click — only reset diabloSubStep when leaving the 'analyse' step
  function handlePanelStepClick(stepId) {
    setActiveStep(stepId);
    if (stepId !== 'analyse') setDiabloSubStep('configure');
  }

  const disabled     = !!fatalError;
  const isDiabloStep = DIABLO_STEPS.has(activeStep);

  return (
    <div style={appStyle}>
      <header style={headerStyle}>
        <button onClick={() => setActiveStep('landing')} style={logoBtnStyle} title="Back to module overview">
          <span style={{ color: '#fff', fontSize: 17, fontWeight: 700, letterSpacing: '-0.01em' }}>InterOmics</span>
        </button>
        {activeStep !== 'landing' && (
          <button onClick={() => setActiveStep('landing')} style={homeLink}>
            {'\u2190'} Module Overview
          </button>
        )}
      </header>

      {activeStep === 'landing' ? (
        <LandingPage onModuleClick={handleModuleClick} onStartPipeline={handleStartPipeline} />

      ) : isDiabloStep ? (
        /* ── DIABLO layout: left panel + right content ─────────────────────── */
        <div style={diabloBodyStyle}>
          <DiabloWorkflowPanel
            activeStep={activeStep}
            diabloSubStep={diabloSubStep}
            pipelineState={pipelineState}
            onStepClick={handlePanelStepClick}
          />
          <main style={diabloMainStyle}>
            {(fatalError || globalError) && (
              <ErrorAlert
                error={fatalError || globalError}
                onDismiss={fatalError ? null : () => setGlobalError(null)}
                onFatal={handleRestartSession}
              />
            )}

            {activeStep === 'upload' && (
              <UploadStep sessionId={sessionId} onComplete={handleUploadComplete} />
            )}
            {activeStep === 'validate' && sessionId && (
              <ValidationStep sessionId={sessionId} onComplete={() => handleStepComplete('comparison')} />
            )}
            {activeStep === 'comparison' && sessionId && (
              <ComparisonStep sessionId={sessionId} onComplete={() => handleStepComplete('preprocess')} />
            )}
            {activeStep === 'preprocess' && sessionId && (
              <PreprocessingStep sessionId={sessionId} onComplete={() => handleStepComplete('analyse')} />
            )}
            {activeStep === 'analyse' && sessionId && (
              <DiabloStep
                sessionId={sessionId}
                onSubStepChange={setDiabloSubStep}
                onComplete={() => handleStepComplete('diablo_results')}
                onReset={handleDiabloReset}
              />
            )}
            {activeStep === 'diablo_results' && sessionId && (
              <DiabloResultsStep
                sessionId={sessionId}
                onRunAgain={handleDiabloReset}
              />
            )}
            {activeStep === 'diablo_export' && sessionId && (
              <DiabloExportStep sessionId={sessionId} />
            )}
          </main>
        </div>

      ) : (
        /* ── Non-DIABLO layout: top PipelineNav + content ───────────────────── */
        <div style={navBodyStyle}>
          <PipelineNav
            activeStep={activeStep}
            pipelineState={pipelineState}
            onStepClick={setActiveStep}
            disabled={disabled}
          />
          <main style={navMainStyle}>
            {(fatalError || globalError) && (
              <ErrorAlert
                error={fatalError || globalError}
                onDismiss={fatalError ? null : () => setGlobalError(null)}
                onFatal={handleRestartSession}
              />
            )}
            {activeStep === 'visualise' && sessionId && (
              <div style={{ display: 'flex', flexDirection: 'column', gap: 32 }}>
                <CorrelationStep sessionId={sessionId} datasets={datasets} />
                <EnrichmentStep  sessionId={sessionId} />
                <StatsStep       sessionId={sessionId} metadataColumns={metadataColumns} />
              </div>
            )}
            {activeStep === 'export' && sessionId && (
              <ExportStep sessionId={sessionId} />
            )}
          </main>
        </div>
      )}
    </div>
  );
}

const appStyle        = { fontFamily: 'system-ui, sans-serif', minHeight: '100vh', display: 'flex', flexDirection: 'column' };
const headerStyle     = { background: '#7b1c2e', color: '#fff', padding: '10px 24px', display: 'flex', alignItems: 'center', justifyContent: 'space-between' };
const logoBtnStyle    = { background: 'none', border: 'none', cursor: 'pointer', padding: 0, display: 'flex', alignItems: 'center', gap: 10 };
const homeLink        = { background: 'none', border: '1px solid rgba(255,255,255,0.35)', color: '#fff', fontSize: 13, cursor: 'pointer', padding: '5px 12px', borderRadius: 4 };
const diabloBodyStyle = { display: 'flex', flex: 1, minHeight: 0 };
const diabloMainStyle = { flex: 1, padding: '24px 28px', overflowY: 'auto' };
const navBodyStyle    = { display: 'flex', flexDirection: 'column', flex: 1 };
const navMainStyle    = { flex: 1, padding: 24, maxWidth: 1100 };

// ─── Error boundary ───────────────────────────────────────────────────────────
export class AppErrorBoundary extends React.Component {
  constructor(props) { super(props); this.state = { error: null }; }
  static getDerivedStateFromError(err) { return { error: err }; }
  render() {
    if (this.state.error) {
      return (
        <div style={{ padding: 32, fontFamily: 'system-ui, sans-serif' }}>
          <div style={{ background: '#fde8e8', border: '1px solid #f5c6cb', borderRadius: 6, padding: '16px 20px', maxWidth: 600 }}>
            <strong style={{ color: '#721c24' }}>Something went wrong</strong>
            <p style={{ color: '#721c24', margin: '6px 0 12px', fontSize: 13 }}>
              {this.state.error.message || String(this.state.error)}
            </p>
            <button
              onClick={() => { this.setState({ error: null }); window.location.reload(); }}
              style={{ padding: '6px 14px', background: '#7b1c2e', color: '#fff', border: 'none', borderRadius: 4, cursor: 'pointer', fontSize: 13 }}
            >
              Reload
            </button>
          </div>
        </div>
      );
    }
    return this.props.children;
  }
}
