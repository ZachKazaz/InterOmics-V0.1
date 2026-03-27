// DiabloStep.jsx — DIABLO Configure + Run sub-steps
// Handles: configure form, job submission, polling, cancel.
// When the job completes it calls onComplete() — the parent (App.jsx)
// navigates to DiabloResultsStep. Results are NOT rendered here.

import { useState, useEffect, useRef } from 'react';
import { apiPost, apiGet } from '../api/session';
import ErrorAlert from './ErrorAlert';
import HelpTooltip from './HelpTooltip';

const C = {
  maroon:      '#7b1c2e',
  border:      '#e2e2e5',
  textPrimary: '#1a1a24',
  textMuted:   '#888896',
  textSec:     '#555563',
  bg:          '#f4f4f5',
  white:       '#ffffff',
  amber:       '#92400e',
  amberBg:     '#fffbeb',
  amberBorder: '#fcd34d',
  redBg:       '#fef2f2',
  redBorder:   '#fca5a5',
  red:         '#991b1b',
};

const POLL_INTERVAL_MS = 5000;
const FALLBACK_KEEPX   = '5, 10, 15, 20';

function parseKeepX(raw) {
  const parts = raw.split(',').map(s => s.trim()).filter(Boolean);
  if (parts.length === 0) return { values: [], error: 'Enter at least one value.' };
  const nums = [];
  for (const p of parts) {
    const n = Number(p);
    if (!Number.isInteger(n) || n <= 0)
      return { values: [], error: `"${p}" is not a valid positive integer.` };
    nums.push(n);
  }
  return { values: [...new Set(nums)].sort((a, b) => a - b), error: null };
}

// viewMode: 'configure' | 'running'
// (results are handled by DiabloResultsStep — not this component)

export default function DiabloStep({ sessionId, onSubStepChange, onComplete, onReset }) {
  const [ncompMax,   setNcompMax]   = useState(2);
  const [keepXStr,   setKeepXStr]   = useState(FALLBACK_KEEPX);
  const [viewMode,   setViewMode]   = useState('configure');
  const [jobStatus,  setJobStatus]  = useState(null);
  const [jobState,   setJobState]   = useState(null);
  const [error,      setError]      = useState(null);
  const [loading,    setLoading]    = useState(false);
  const [actionBusy, setActionBusy] = useState(false);
  const [stalled,    setStalled]    = useState(false);

  const pollRef        = useRef(null);
  const lastUpdatedRef = useRef(null);
  const stallTimerRef  = useRef(null);

  // Sync parent sub-step label
  useEffect(() => {
    if (!onSubStepChange) return;
    onSubStepChange(viewMode === 'running' ? 'run' : 'configure');
  }, [viewMode, onSubStepChange]);

  useEffect(() => () => {
    clearInterval(pollRef.current);
    clearTimeout(stallTimerRef.current);
  }, []);

  // On mount: check for an existing job.
  // IMPORTANT: if the job is 'complete' we do NOT auto-navigate to results.
  // The user must explicitly click "View Results" or run again.
  // This prevents stale results from a prior upload cycle from hijacking navigation.
  useEffect(() => {
    async function checkExistingJob() {
      try {
        const data = await apiGet(`/${sessionId}/analysis/diablo/status`);
        if (!data || !data.status || data.error_code === 'NO_FILES_FOUND') {
          console.log('[DIABLO routing] no existing job found — clean configure state');
          return;
        }
        console.log('[DIABLO routing] existing job status on mount:', data.status);
        setJobState(data);
        setJobStatus(data.status);
        if (data.status === 'running' || data.status === 'starting') {
          setViewMode('running');
          setLoading(true);
          startPolling();
        }
        // complete / cancelled / error → stay on configure, show terminal banner
        // Do NOT call onComplete() here — that would skip Configure entirely
      } catch {
        console.log('[DIABLO routing] status check failed — treating as no job');
      }
    }
    if (sessionId) checkExistingJob();
  // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [sessionId]);

  // Fetch keepX recommendation from comparison candidates
  useEffect(() => {
    async function fetchRecommendation() {
      try {
        const data = await apiGet(`/${sessionId}/comparison/candidates`);
        if (data?.keepx_default_string) setKeepXStr(data.keepx_default_string);
      } catch { /* non-fatal */ }
    }
    if (sessionId) fetchRecommendation();
  }, [sessionId]);

  // ── Submit ──────────────────────────────────────────────────────────────────
  async function handleSubmit(e) {
    e.preventDefault();
    const { values, error: parseErr } = parseKeepX(keepXStr);
    if (parseErr) return;

    setError(null);
    setLoading(true);
    try {
      const data = await apiPost(`/${sessionId}/analysis/diablo`, {
        ncomp_max:  ncompMax,
        keepX_grid: values,
        cv_folds:   5,
        cv_repeats: 10,
      });
      if (data.status === 'error') { setError(data); setLoading(false); return; }
      if (!data.job_id && data.status !== 'running') {
        setError({ error_code: 'UNEXPECTED_RESPONSE', detail: 'Unexpected response from server.', recoverable: true });
        setLoading(false);
        return;
      }
      setJobStatus('running');
      setJobState(data);
      setViewMode('running');
      startPolling();
    } catch (err) {
      setError({ error_code: 'NETWORK_ERROR', detail: String(err.message ?? err), recoverable: true });
      setLoading(false);
    }
  }

  // ── Run Again Now (from terminal banner — cancelled/error states) ───────────
  async function handleRunAgainNow() {
    const { values, error: parseErr } = parseKeepX(keepXStr);
    if (parseErr) return;
    setActionBusy(true);
    setError(null);
    console.log('[DIABLO routing] handleRunAgainNow — submitting restart');
    try {
      const data = await apiPost(`/${sessionId}/analysis/diablo/restart`, {
        ncomp_max:  ncompMax,
        keepX_grid: values,
        cv_folds:   5,
        cv_repeats: 10,
      });
      if (data.status === 'error') { setError(data); return; }
      setJobStatus('running');
      setJobState(data);
      setLoading(true);
      setViewMode('running');
      startPolling();
    } catch (err) {
      setError({ error_code: 'NETWORK_ERROR', detail: String(err.message ?? err), recoverable: true });
    } finally {
      setActionBusy(false);
    }
  }

  // ── Cancel ──────────────────────────────────────────────────────────────────
  async function handleCancel() {
    setActionBusy(true);
    try {
      const data = await apiPost(`/${sessionId}/analysis/diablo/cancel`, {});
      if (data.status === 'error') {
        setError(data);
      } else {
        clearInterval(pollRef.current);
        clearTimeout(stallTimerRef.current);
        setJobStatus('cancelled');
        setJobState(prev => ({ ...(prev || {}), status: 'cancelled', stage_label: 'Cancelled by user' }));
        setLoading(false);
        setViewMode('configure');
      }
    } catch (err) {
      setError({ error_code: 'NETWORK_ERROR', detail: String(err.message ?? err), recoverable: true });
    } finally {
      setActionBusy(false);
    }
  }

  function startPolling() {
    clearInterval(pollRef.current);
    clearTimeout(stallTimerRef.current);
    lastUpdatedRef.current = null;
    setStalled(false);
    pollRef.current = setInterval(async () => {
      try {
        const data = await apiGet(`/${sessionId}/analysis/diablo/status`);
        const s = data.status ?? data.data?.status;
        setJobState(data);
        setJobStatus(s);

        const newUpdatedAt = data.updated_at;
        if (newUpdatedAt && newUpdatedAt !== lastUpdatedRef.current) {
          lastUpdatedRef.current = newUpdatedAt;
          setStalled(false);
          clearTimeout(stallTimerRef.current);
          if (s === 'running' || s === 'starting') {
            stallTimerRef.current = setTimeout(() => setStalled(true), 60_000);
          }
        }

        if (s === 'complete') {
          clearInterval(pollRef.current);
          clearTimeout(stallTimerRef.current);
          setLoading(false);
          // Navigate to results page — DiabloResultsStep handles display
          if (onComplete) onComplete();
        } else if (s === 'error') {
          clearInterval(pollRef.current);
          clearTimeout(stallTimerRef.current);
          setLoading(false);
          setError({
            error_code:  data.error_code || 'ANALYSIS_RUNTIME_ERROR',
            detail:      data.detail     || 'DIABLO analysis failed.',
            recoverable: true,
          });
          setViewMode('configure');
        } else if (s === 'cancelled') {
          clearInterval(pollRef.current);
          clearTimeout(stallTimerRef.current);
          setLoading(false);
          setViewMode('configure');
        }
      } catch { /* transient poll failure — keep polling */ }
    }, POLL_INTERVAL_MS);
  }

  const { values: parsedKeepX, error: keepXParseErr } = parseKeepX(keepXStr);
  const submitDisabled = loading || !!keepXParseErr || parsedKeepX.length === 0;
  const showTerminalBanner =
    viewMode === 'configure' && jobStatus != null &&
    jobStatus !== 'running' && !error;

  const pageTitle = viewMode === 'running' ? 'Run Model' : 'Configure DIABLO';

  return (
    <div>
      <h2 style={headingStyle}>{pageTitle}</h2>
      <ErrorAlert error={error} onDismiss={() => setError(null)} />

      {/* ── CONFIGURE VIEW ─────────────────────────────────────────────────── */}
      {viewMode === 'configure' && (
        <>
          {showTerminalBanner && (
            <TerminalJobBanner
              status={jobStatus}
              jobState={jobState}
              actionBusy={actionBusy}
              onViewResults={jobStatus === 'complete' ? onComplete : null}
              onReset={onReset}
              onRunAgainNow={handleRunAgainNow}
            />
          )}

          <form onSubmit={handleSubmit} style={{ maxWidth: 560 }}>
            <fieldset style={fsStyle}>
              <legend style={legendStyle}>Model Parameters</legend>

              <label style={rowStyle}>
                <span style={labelTextStyle}>
                  Max components (ncomp_max)
                  <HelpTooltip text="Maximum number of DIABLO latent components to test. 2–3 is typical; more components increase computation time and overfitting risk." />
                </span>
                <input
                  type="number" min={1} max={5} value={ncompMax}
                  onChange={e => setNcompMax(Math.max(1, parseInt(e.target.value) || 1))}
                  style={numInputStyle}
                />
              </label>

              <div style={{ marginTop: 16 }}>
                <div style={{ ...labelTextStyle, marginBottom: 4 }}>
                  keepX grid
                  <HelpTooltip text="Comma-separated list of feature counts per block/component to evaluate during cross-validation tuning. The best combination is selected automatically." />
                </div>
                <input
                  type="text"
                  value={keepXStr}
                  onChange={e => setKeepXStr(e.target.value)}
                  placeholder="e.g. 5, 10, 15, 20"
                  style={textInputStyle(!!keepXParseErr)}
                  aria-label="keepX grid values"
                />
                <p style={{ fontSize: 12, color: C.textMuted, margin: '4px 0 0' }}>
                  Comma-separated positive integers — features per block/component evaluated during tuning.
                </p>
                {keepXParseErr && (
                  <p style={{ fontSize: 12, color: '#c0392b', margin: '4px 0 0' }}>{keepXParseErr}</p>
                )}
                {!keepXParseErr && parsedKeepX.length > 0 && (
                  <p style={{ fontSize: 12, color: C.textMuted, margin: '4px 0 0' }}>
                    Will test: {parsedKeepX.join(', ')}
                  </p>
                )}
              </div>

              <div style={{ marginTop: 14, fontSize: 13, color: C.textMuted }}>
                CV folds: <strong style={{ color: C.textSec }}>5</strong> (fixed)
                &nbsp;·&nbsp;
                CV repeats: <strong style={{ color: C.textSec }}>10</strong> (fixed)
                &nbsp;·&nbsp;
                Design off-diagonal: <strong style={{ color: C.textSec }}>0.1</strong> (fixed)
              </div>
            </fieldset>

            <p style={guidanceStyle}>
              <strong>ncomp_max</strong> sets the maximum number of components to extract.
              The <strong>keepX grid</strong> defines how many features per block are selected per component —
              cross-validation picks the best combination from this list.
            </p>

            <button type="submit" disabled={submitDisabled} style={btnStyle(submitDisabled)}>
              Run DIABLO
            </button>
          </form>
        </>
      )}

      {/* ── RUNNING VIEW ───────────────────────────────────────────────────── */}
      {viewMode === 'running' && (
        <div style={{ display: 'flex', flexDirection: 'column', alignItems: 'center', width: '100%' }}>
          <div style={{ width: '100%', display: 'flex', justifyContent: 'center' }}>
            <RunningBanner jobState={jobState} stalled={stalled} />
          </div>
          <button
            onClick={handleCancel}
            disabled={actionBusy}
            style={{ ...btnStyle(actionBusy), background: actionBusy ? '#bbb' : C.amber, marginTop: 12 }}
          >
            {actionBusy ? 'Cancelling…' : 'Cancel Job'}
          </button>
        </div>
      )}
    </div>
  );
}

// ─── Terminal job banner ──────────────────────────────────────────────────────
// Shown on the Configure view when a prior job exists (complete/cancelled/error).
// For 'complete': offers "View Results" (navigate) and "Reset & Reconfigure" (clear state).
// For 'cancelled'/'error': offers "Reset & Reconfigure" and "Run Again Now".
function TerminalJobBanner({ status, jobState, actionBusy, onViewResults, onReset, onRunAgainNow }) {
  const isComplete  = status === 'complete';
  const isCancelled = status === 'cancelled';
  const isError     = status === 'error';
  const bg     = isComplete ? '#f0fdf4' : isError ? '#fef2f2'  : '#fffbeb';
  const border = isComplete ? '#86efac' : isError ? '#fca5a5'  : '#fcd34d';
  const color  = isComplete ? '#166534' : isError ? '#991b1b'  : '#92400e';
  const title  = isComplete  ? 'A previous DIABLO run completed for this session' :
                 isCancelled ? 'Previous run was cancelled' : 'Previous run failed';
  const detail = isError && jobState?.detail ? jobState.detail :
                 !isComplete && jobState?.stage_label &&
                 !['Cancelled by user', 'Failed'].includes(jobState.stage_label)
                   ? `Last stage: ${jobState.stage_label}` : null;

  return (
    <div style={{ background: bg, border: `1px solid ${border}`, borderRadius: 6,
                  padding: '12px 16px', maxWidth: 520, marginBottom: 20 }}>
      <div style={{ display: 'flex', alignItems: 'flex-start', gap: 10 }}>
        <span style={{ fontSize: 16 }}>{isComplete ? '✓' : isError ? '✗' : '↩'}</span>
        <div style={{ flex: 1 }}>
          <div style={{ fontWeight: 600, fontSize: 13, color }}>{title}</div>
          {detail && (
            <div style={{ fontSize: 12, color: '#888896', marginTop: 3,
                          fontFamily: isError ? 'monospace' : 'inherit' }}>
              {detail}
            </div>
          )}
          {isComplete && (
            <div style={{ fontSize: 12, color: '#555563', marginTop: 4 }}>
              You can view the existing results, or reset and run a new analysis with different parameters.
            </div>
          )}
          <div style={{ display: 'flex', gap: 8, marginTop: 10, flexWrap: 'wrap' }}>
            {isComplete && onViewResults && (
              <ActionBtn onClick={onViewResults} disabled={actionBusy} variant="primary">
                View Results →
              </ActionBtn>
            )}
            {onReset && (
              <ActionBtn onClick={onReset} disabled={actionBusy} variant="secondary">
                {actionBusy ? 'Resetting…' : isComplete ? 'Reset & Reconfigure' : 'Reset & Reconfigure'}
              </ActionBtn>
            )}
            {!isComplete && (
              <ActionBtn onClick={onRunAgainNow} disabled={actionBusy} variant="primary">
                {actionBusy ? 'Starting…' : 'Run Again Now'}
              </ActionBtn>
            )}
          </div>
        </div>
      </div>
    </div>
  );
}

// ─── Running banner ───────────────────────────────────────────────────────────
function RunningBanner({ jobState, stalled }) {
  const pct        = jobState?.progress_percent ?? 0;
  const label      = jobState?.stage_label ?? 'Running…';
  const isStarting = jobState?.status === 'starting';
  return (
    <div style={{ display: 'flex', alignItems: 'flex-start', gap: 12,
                  background: '#f4f4f5', border: '1px solid #e2e2e5',
                  borderRadius: 6, padding: '12px 16px', maxWidth: 480 }}>
      <span style={{ fontSize: 18 }}>{isStarting ? '🔄' : '⏳'}</span>
      <div style={{ flex: 1 }}>
        <div style={{ fontWeight: 600, fontSize: 14 }}>{label}</div>
        <div style={{ fontSize: 12, color: '#888896', marginTop: 2 }}>
          {isStarting
            ? 'Launching DIABLO worker…'
            : 'DIABLO cross-validation can take several minutes. Polling every 5 s…'}
        </div>
        <ProgressBar pct={pct} />
        {jobState?.updated_at && (
          <div style={{ fontSize: 11, color: '#888896', marginTop: 4 }}>
            Last update: {jobState.updated_at}
          </div>
        )}
        {stalled && (
          <div style={{ marginTop: 8, fontSize: 12, color: '#92400e',
                        background: '#fffbeb', border: '1px solid #fcd34d',
                        borderRadius: 4, padding: '6px 10px' }}>
            ⚠ Job appears stalled — no progress update in over 60 s.
            Check <code>storage/&lt;session&gt;/jobs/diablo_worker.log</code> for details.
          </div>
        )}
      </div>
    </div>
  );
}

function ProgressBar({ pct }) {
  const safe = Math.min(100, Math.max(0, pct || 0));
  return (
    <div style={{ marginTop: 8, background: '#e5e7eb', borderRadius: 4, height: 6, overflow: 'hidden' }}>
      <div style={{ width: `${safe}%`, background: '#7b1c2e', height: '100%', transition: 'width 0.4s ease' }} />
    </div>
  );
}

function ActionBtn({ onClick, disabled, variant, children }) {
  const bg = variant === 'primary' ? '#7b1c2e' : '#6b7280';
  return (
    <button onClick={onClick} disabled={disabled} style={{
      padding: '6px 14px',
      background: disabled ? '#d1d5db' : bg,
      color: '#fff', border: 'none', borderRadius: 4,
      cursor: disabled ? 'not-allowed' : 'pointer',
      fontWeight: 600, fontSize: 13,
    }}>
      {children}
    </button>
  );
}

// ─── Styles ───────────────────────────────────────────────────────────────────
const headingStyle   = { fontSize: 20, fontWeight: 700, color: '#1a1a24', margin: '0 0 16px' };
const fsStyle        = { border: '1px solid #e2e2e5', borderRadius: 6, padding: '12px 16px', marginBottom: 12 };
const legendStyle    = { fontSize: 13, fontWeight: 700, color: '#1a1a24', padding: '0 4px' };
const rowStyle       = { display: 'flex', alignItems: 'center', gap: 8, fontSize: 13 };
const labelTextStyle = { display: 'flex', alignItems: 'center', gap: 4, fontSize: 13, fontWeight: 600, color: '#1a1a24' };
const numInputStyle  = { width: 64, padding: '4px 6px', border: '1px solid #e2e2e5', borderRadius: 4, fontSize: 13 };
const textInputStyle = hasErr => ({
  width: '100%', padding: '6px 10px',
  border: `1px solid ${hasErr ? '#c0392b' : '#e2e2e5'}`,
  borderRadius: 4, fontSize: 13, fontFamily: 'monospace', boxSizing: 'border-box',
});
const guidanceStyle  = { fontSize: 12, color: '#888896', lineHeight: 1.5, marginBottom: 14, maxWidth: 540 };
const btnStyle = disabled => ({
  padding: '9px 24px',
  background: disabled ? '#bbb' : '#7b1c2e',
  color: '#fff', border: 'none', borderRadius: 5,
  cursor: disabled ? 'not-allowed' : 'pointer',
  fontWeight: 600, fontSize: 14,
});
