// src/components/PipelineNav.jsx — Pipeline navigation
// Maroon/white/gray design system.

import React from 'react';

const STEPS = [
  { id: 'upload',      label: 'Upload',     prerequisite: null },
  { id: 'validate',    label: 'Validate',   prerequisite: 'upload' },
  { id: 'preprocess',  label: 'Preprocess', prerequisite: 'validate' },
  { id: 'analyse',     label: 'Analyse',    prerequisite: 'preprocess' },
  { id: 'visualise',   label: 'Visualise',  prerequisite: 'preprocess' },
  { id: 'export',      label: 'Export',     prerequisite: 'preprocess' },
];

const C = {
  maroon:      '#7b1c2e',
  maroonDark:  '#5e1522',
  maroonLight: '#f5eaec',
  complete:    '#2d6a4f',
  completeLight: '#d8f3dc',
  error:       '#c0392b',
  errorLight:  '#fdecea',
  disabled:    '#e8e8ec',
  disabledText:'#aaaabc',
  border:      '#e2e2e5',
  text:        '#1a1a24',
};

export default function PipelineNav({ pipelineState, activeStep, onStepClick, disabled }) {
  const steps = pipelineState?.steps || {};

  function isEnabled(step) {
    if (disabled) return false;
    if (step.prerequisite === null) return true;
    return steps[step.prerequisite] === 'complete';
  }

  function getStatus(step) {
    const s = steps[step.id];
    if (s === 'complete') return 'complete';
    if (s === 'error')    return 'error';
    return 'pending';
  }

  return (
    <nav aria-label="Pipeline steps" style={{ display: 'flex', gap: 0, margin: '16px 24px 0', borderRadius: 6, overflow: 'hidden', border: `1px solid ${C.border}` }}>
      {STEPS.map((step, idx) => {
        const enabled  = isEnabled(step);
        const status   = getStatus(step);
        const isActive = step.id === activeStep;

        let bg = C.disabled;
        let color = C.disabledText;
        let borderLeft = idx === 0 ? 'none' : `1px solid ${C.border}`;

        if (isActive) {
          bg = C.maroon; color = '#fff'; borderLeft = idx === 0 ? 'none' : `1px solid ${C.maroonDark}`;
        } else if (status === 'complete') {
          bg = C.completeLight; color = C.complete;
        } else if (status === 'error') {
          bg = C.errorLight; color = C.error;
        } else if (enabled) {
          bg = '#fff'; color = C.text;
        }

        return (
          <button
            key={step.id}
            onClick={() => enabled && onStepClick(step.id)}
            disabled={!enabled}
            aria-current={isActive ? 'step' : undefined}
            title={!enabled ? `Complete "${STEPS[idx - 1]?.label}" first` : step.label}
            style={{
              flex: 1,
              padding: '10px 4px',
              background: bg,
              color,
              border: 'none',
              borderLeft,
              cursor: enabled ? 'pointer' : 'not-allowed',
              fontWeight: isActive ? 700 : 500,
              fontSize: 12,
              display: 'flex',
              flexDirection: 'column',
              alignItems: 'center',
              gap: 2,
              transition: 'background 0.12s ease',
            }}
          >
            <span>
              {status === 'complete' && '✓ '}
              {status === 'error'    && '✕ '}
              {step.label}
            </span>
          </button>
        );
      })}
    </nav>
  );
}
