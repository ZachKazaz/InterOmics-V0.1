// src/components/EducationPanel.jsx — Collapsible education content panel
// Task 4.12

import React, { useState } from 'react';

/**
 * Props:
 *   education — { what_it_does, when_to_use, input_requirements,
 *                 output_interpretation, common_mistakes[] }
 *               or null/undefined (panel is hidden)
 */
export default function EducationPanel({ education }) {
  const [open, setOpen] = useState(false);

  if (!education) return null;

  const sections = [
    { label: 'What it does',          value: education.what_it_does },
    { label: 'When to use',           value: education.when_to_use },
    { label: 'Input requirements',    value: education.input_requirements },
    { label: 'Output interpretation', value: education.output_interpretation },
  ];

  return (
    <div
      style={{
        border: '1px solid #b8daff',
        borderRadius: 4,
        marginBottom: 16,
        background: '#e8f4fd',
      }}
    >
      <button
        onClick={() => setOpen(o => !o)}
        aria-expanded={open}
        style={{
          width: '100%',
          textAlign: 'left',
          padding: '10px 14px',
          background: 'none',
          border: 'none',
          cursor: 'pointer',
          fontWeight: 600,
          color: '#004085',
          display: 'flex',
          justifyContent: 'space-between',
        }}
      >
        <span>ℹ About this step</span>
        <span>{open ? '▲' : '▼'}</span>
      </button>

      {open && (
        <div style={{ padding: '0 14px 14px' }}>
          {sections.map(({ label, value }) =>
            value ? (
              <div key={label} style={{ marginBottom: 10 }}>
                <strong style={{ color: '#004085' }}>{label}</strong>
                <p style={{ margin: '2px 0 0', color: '#333' }}>{value}</p>
              </div>
            ) : null
          )}

          {education.common_mistakes && education.common_mistakes.length > 0 && (
            <div>
              <strong style={{ color: '#004085' }}>Common mistakes</strong>
              <ul style={{ margin: '4px 0 0', paddingLeft: 20, color: '#333' }}>
                {education.common_mistakes.map((m, i) => (
                  <li key={i}>{m}</li>
                ))}
              </ul>
            </div>
          )}
        </div>
      )}
    </div>
  );
}
