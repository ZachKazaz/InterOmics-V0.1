// src/components/Breadcrumb.jsx — Horizontal breadcrumb navigation

import React from 'react';

const C = {
  maroon:    '#7b1c2e',
  muted:     '#888896',
  text:      '#1a1a24',
  border:    '#e2e2e5',
};

/**
 * Props:
 *   crumbs — array of { label: string, onClick?: () => void }
 *            Last crumb is the current location (non-clickable).
 */
export default function Breadcrumb({ crumbs }) {
  if (!crumbs || crumbs.length === 0) return null;
  return (
    <nav aria-label="Breadcrumb" style={{ display: 'flex', alignItems: 'center', gap: 6, marginBottom: 20, flexWrap: 'wrap' }}>
      {crumbs.map((crumb, i) => {
        const isLast = i === crumbs.length - 1;
        return (
          <React.Fragment key={i}>
            {isLast ? (
              <span style={{ fontSize: 13, color: C.text, fontWeight: 600 }} aria-current="page">
                {crumb.label}
              </span>
            ) : (
              <button
                onClick={crumb.onClick}
                style={{
                  background: 'none',
                  border: 'none',
                  padding: 0,
                  fontSize: 13,
                  color: C.maroon,
                  cursor: crumb.onClick ? 'pointer' : 'default',
                  textDecoration: crumb.onClick ? 'underline' : 'none',
                  textUnderlineOffset: 2,
                }}
              >
                {crumb.label}
              </button>
            )}
            {!isLast && (
              <span style={{ fontSize: 12, color: C.muted, userSelect: 'none' }}>›</span>
            )}
          </React.Fragment>
        );
      })}
    </nav>
  );
}
