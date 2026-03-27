// src/components/HelpTooltip.jsx — Inline expandable help tooltip

import React, { useState, useRef, useEffect } from 'react';

const C = {
  maroon:    '#7b1c2e',
  maroonLight: '#f5eaec',
  border:    '#e2e2e5',
  text:      '#1a1a24',
  muted:     '#555563',
};

/**
 * Props:
 *   text — string: help content to display when expanded
 */
export default function HelpTooltip({ text }) {
  const [open, setOpen] = useState(false);
  const ref = useRef(null);

  // Close on outside click
  useEffect(() => {
    if (!open) return;
    function handler(e) {
      if (ref.current && !ref.current.contains(e.target)) setOpen(false);
    }
    document.addEventListener('mousedown', handler);
    return () => document.removeEventListener('mousedown', handler);
  }, [open]);

  return (
    <span ref={ref} style={{ position: 'relative', display: 'inline-flex', alignItems: 'center', marginLeft: 5 }}>
      <button
        type="button"
        aria-label="Help"
        aria-expanded={open}
        onClick={() => setOpen(o => !o)}
        style={{
          width: 16,
          height: 16,
          borderRadius: '50%',
          border: `1.5px solid ${C.maroon}`,
          background: open ? C.maroon : 'transparent',
          color: open ? '#fff' : C.maroon,
          fontSize: 10,
          fontWeight: 700,
          cursor: 'pointer',
          display: 'inline-flex',
          alignItems: 'center',
          justifyContent: 'center',
          padding: 0,
          lineHeight: 1,
          flexShrink: 0,
          transition: 'background 0.12s ease, color 0.12s ease',
        }}
      >
        ?
      </button>

      {open && (
        <div
          role="tooltip"
          style={{
            position: 'absolute',
            top: 22,
            left: 0,
            zIndex: 100,
            background: C.maroonLight,
            border: `1px solid ${C.border}`,
            borderRadius: 6,
            padding: '10px 12px',
            minWidth: 220,
            maxWidth: 300,
            fontSize: 12,
            color: C.muted,
            lineHeight: 1.55,
            boxShadow: '0 4px 16px rgba(0,0,0,0.10)',
          }}
        >
          {text}
        </div>
      )}
    </span>
  );
}
