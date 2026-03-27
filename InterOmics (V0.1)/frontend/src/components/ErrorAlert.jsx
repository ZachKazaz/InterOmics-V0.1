// src/components/ErrorAlert.jsx — Global API error display
// Task 4.13

import React from 'react';
import { clearSession } from '../api/session';

/**
 * Renders a dismissible error alert.
 * If recoverable=false: disables pipeline nav and shows "Start New Session" button.
 *
 * Props:
 *   error       — { status, error_code, detail, recoverable } or null
 *   onDismiss   — called when user dismisses a recoverable error
 *   onFatal     — called after session is cleared (for non-recoverable errors)
 */
export default function ErrorAlert({ error, onDismiss, onFatal }) {
  if (!error) return null;

  const isFatal = error.recoverable === false;

  function handleRestart() {
    clearSession();
    if (onFatal) onFatal();
    window.location.reload();
  }

  return (
    <div
      role="alert"
      style={{
        background: isFatal ? '#fde8e8' : '#fff3cd',
        border: `1px solid ${isFatal ? '#f5c6cb' : '#ffc107'}`,
        borderRadius: 4,
        padding: '12px 16px',
        marginBottom: 16,
        display: 'flex',
        justifyContent: 'space-between',
        alignItems: 'flex-start',
      }}
    >
      <div>
        <strong style={{ color: isFatal ? '#721c24' : '#856404' }}>
          {error.error_code}
        </strong>
        <p style={{ margin: '4px 0 0', color: isFatal ? '#721c24' : '#533f03' }}>
          {error.detail}
        </p>
        {isFatal && (
          <button
            onClick={handleRestart}
            style={{
              marginTop: 10,
              padding: '6px 14px',
              background: '#dc3545',
              color: '#fff',
              border: 'none',
              borderRadius: 4,
              cursor: 'pointer',
            }}
          >
            Start New Session
          </button>
        )}
      </div>
      {!isFatal && onDismiss && (
        <button
          onClick={onDismiss}
          aria-label="Dismiss error"
          style={{
            background: 'none',
            border: 'none',
            fontSize: 18,
            cursor: 'pointer',
            color: '#856404',
            lineHeight: 1,
          }}
        >
          ×
        </button>
      )}
    </div>
  );
}
