// DiabloWorkflowPanel.jsx — Vertical left-side DIABLO workflow navigation
// Single source of truth for all DIABLO step navigation.
// 8 steps: Upload → Validate → Define Comparison → Preprocess →
//          Configure DIABLO → Run Model → View Results → Export Results

import React from 'react';

const C = {
  maroon:      '#7b1c2e',
  border:      '#e2e2e5',
  textPrimary: '#1a1a24',
  textMuted:   '#aaaabc',
  textSec:     '#555563',
  bg:          '#f9f9fb',
  white:       '#ffffff',
  green:       '#2d6a4f',
  activeBg:    '#f0e8ea',
};

// Ordered DIABLO workflow steps.
// id = App-level step id used in setActiveStep / DIABLO_STEPS.
// prereq = id of the step that must be complete before this one is enabled.
export const DIABLO_NAV_STEPS = [
  { id: 'upload',        label: 'Upload Data',       prereq: null },
  { id: 'validate',      label: 'Validate Inputs',   prereq: 'upload' },
  { id: 'comparison',    label: 'Define Comparison', prereq: 'validate' },
  { id: 'preprocess',    label: 'Preprocess Data',   prereq: 'comparison' },
  { id: 'analyse',       label: 'Configure DIABLO',  prereq: 'preprocess' },
  { id: 'run',           label: 'Run Model',          prereq: 'analyse' },
  { id: 'diablo_results',label: 'View Results',       prereq: 'run' },
  { id: 'diablo_export', label: 'Export Results',     prereq: 'diablo_results' },
];

// Map App-level activeStep + DiabloStep internal sub-state → panel highlight id.
export function resolveActivePanelStep(activeStep, diabloSubStep) {
  if (activeStep === 'diablo_results') return 'diablo_results';
  if (activeStep === 'diablo_export')  return 'diablo_export';
  if (activeStep !== 'analyse') return activeStep;
  // activeStep === 'analyse': sub-step drives the highlight
  if (diabloSubStep === 'run')     return 'run';
  if (diabloSubStep === 'results') return 'diablo_results'; // results sub-step → highlight View Results
  return 'analyse';
}

/**
 * Props:
 *   activeStep    — App-level step id
 *   diabloSubStep — 'configure'|'run'|'results'
 *   pipelineState — { steps: { upload, validate, comparison, preprocess, diablo, … } }
 *   onStepClick   — (appStepId: string) => void
 */
export default function DiabloWorkflowPanel({ activeStep, diabloSubStep, pipelineState, onStepClick }) {
  const steps           = pipelineState?.steps || {};
  const activePanelStep = resolveActivePanelStep(activeStep, diabloSubStep);

  // Map panel step ids → completed boolean
  const completedMap = {
    upload:         steps.upload      === 'complete',
    validate:       steps.validate    === 'complete',
    comparison:     steps.comparison  === 'complete',
    preprocess:     steps.preprocess  === 'complete',
    analyse:        steps.diablo      === 'complete',
    run:            steps.diablo      === 'complete',
    diablo_results: steps.diablo      === 'complete',
    diablo_export:  steps.diablo      === 'complete',
  };

  function isEnabled(step) {
    if (step.prereq === null) return true;
    return completedMap[step.prereq] === true;
  }

  // Map panel step id → App-level step id for navigation.
  // 'run' is a sub-state of 'analyse' — clicking it navigates to 'analyse'.
  function toAppStep(panelId) {
    if (panelId === 'run') return 'analyse';
    return panelId;
  }

  return (
    <nav
      aria-label="DIABLO workflow steps"
      style={{
        width: 200, flexShrink: 0,
        borderRight: `1px solid ${C.border}`,
        background: C.bg,
        display: 'flex', flexDirection: 'column',
        padding: '20px 0',
      }}
    >
      <div style={{
        fontSize: 10, fontWeight: 700, color: C.textMuted,
        textTransform: 'uppercase', letterSpacing: '0.08em',
        padding: '0 16px 12px',
        borderBottom: `1px solid ${C.border}`, marginBottom: 8,
      }}>
        DIABLO Workflow
      </div>

      {DIABLO_NAV_STEPS.map((step, i) => {
        const isActive    = step.id === activePanelStep;
        const isCompleted = completedMap[step.id];
        const enabled     = isEnabled(step);

        let bg       = 'transparent';
        let color    = C.textMuted;
        let numBg    = C.border;
        let numColor = C.textMuted;
        let fw       = 400;
        let cursor   = 'not-allowed';

        if (isActive) {
          bg = C.activeBg; color = C.maroon;
          numBg = C.maroon; numColor = C.white;
          fw = 700; cursor = 'pointer';
        } else if (isCompleted) {
          color = C.green; numBg = C.green; numColor = C.white;
          fw = 500; cursor = 'pointer';
        } else if (enabled) {
          color = C.textPrimary; numBg = C.border; numColor = C.textSec;
          fw = 400; cursor = 'pointer';
        }

        return (
          <button
            key={step.id}
            onClick={() => enabled && onStepClick(toAppStep(step.id))}
            disabled={!enabled}
            aria-current={isActive ? 'step' : undefined}
            title={!enabled ? 'Complete previous steps first' : step.label}
            style={{
              display: 'flex', alignItems: 'center', gap: 10,
              padding: '9px 16px',
              background: bg, border: 'none',
              borderLeft: isActive ? `3px solid ${C.maroon}` : '3px solid transparent',
              cursor, textAlign: 'left', width: '100%',
              color, fontWeight: fw, fontSize: 13,
              transition: 'background 0.1s ease',
            }}
          >
            <span style={{
              width: 20, height: 20, borderRadius: '50%', flexShrink: 0,
              display: 'inline-flex', alignItems: 'center', justifyContent: 'center',
              fontSize: 10, fontWeight: 700,
              background: numBg, color: numColor,
            }}>
              {isCompleted && !isActive ? '✓' : i + 1}
            </span>
            {step.label}
          </button>
        );
      })}
    </nav>
  );
}
