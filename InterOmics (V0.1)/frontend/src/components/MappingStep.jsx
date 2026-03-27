// src/components/MappingStep.jsx — Feature ID mapping step
// Task 4.6

import React, { useEffect, useState } from 'react';
import { apiPost } from '../api/session';
import ErrorAlert from './ErrorAlert';
import Breadcrumb from './Breadcrumb';

/**
 * Props:
 *   sessionId  — string
 *   onComplete — (mappingResult) => void
 */
export default function MappingStep({ sessionId, onComplete, onGoHome }) {
  const [result, setResult]   = useState(null);
  const [error, setError]     = useState(null);
  const [loading, setLoading] = useState(true);
  const [unmappedOpen, setUnmappedOpen] = useState(false);

  useEffect(() => {
    async function runMapping() {
      try {
        const data = await apiPost(`/${sessionId}/map-ids`, {});
        if (data.status === 'error') {
          setError(data);
        } else {
          setResult(data.data || data);
          onComplete(data.data || data);
        }
      } catch (err) {
        setError({ error_code: 'NETWORK_ERROR', detail: err.message, recoverable: true });
      } finally {
        setLoading(false);
      }
    }
    runMapping();
  }, [sessionId]);

  const rate = result?.mapping_success_rate_pct ?? result?.data?.mapping_success_rate_pct;
  const warning = result?.warning ?? result?.data?.warning;
  const unmapped = result?.unmapped_features ?? result?.data?.unmapped_features ?? [];

  return (
    <div>
      <Breadcrumb crumbs={[
        { label: 'Home', onClick: onGoHome },
        { label: 'Map Feature IDs' },
      ]} />

      <h2 style={headingStyle}>Map Feature IDs</h2>

      <OutputsBox outputs={[
        'KEGG compound IDs mapped to your feature names',
        'Mapping success rate and list of unmapped features',
      ]} />

      {loading && <p style={{ color: '#555563', fontSize: 14 }}>Running ID mapping…</p>}

      <ErrorAlert error={error} onDismiss={() => setError(null)} />

      {result && (
        <div>
          <p>
            <strong>Mapping success rate:</strong>{' '}
            <span style={{ fontSize: 18, fontWeight: 700, color: rate >= 50 ? '#2d6a4f' : '#c0392b' }}>
              {rate != null ? `${Number(rate).toFixed(1)}%` : '—'}
            </span>
          </p>

          {warning && (
            <div role="alert" style={warnStyle}>⚠ {warning}</div>
          )}

          {unmapped.length > 0 && (
            <div style={{ marginBottom: 12 }}>
              <button
                onClick={() => setUnmappedOpen(o => !o)}
                style={{ background: 'none', border: 'none', cursor: 'pointer', color: '#7b1c2e', padding: 0, fontSize: 13 }}
              >
                {unmappedOpen ? '▲' : '▼'} {unmapped.length} unmapped feature{unmapped.length !== 1 ? 's' : ''}
              </button>
              {unmappedOpen && (
                <ul style={{ marginTop: 6, paddingLeft: 20, fontSize: 13, maxHeight: 200, overflowY: 'auto' }}>
                  {unmapped.map(f => <li key={f}>{f}</li>)}
                </ul>
              )}
            </div>
          )}
        </div>
      )}
    </div>
  );
}

const headingStyle = { fontSize: 20, fontWeight: 700, color: '#1a1a24', marginBottom: 16, marginTop: 0 };
const warnStyle = { background: '#fff3cd', border: '1px solid #ffc107', borderRadius: 4, padding: '8px 12px', marginBottom: 12, color: '#856404', fontSize: 13 };

function OutputsBox({ outputs }) {
  return (
    <div style={{ background: '#f5eaec', border: '1px solid #e2c8cc', borderRadius: 6, padding: '10px 14px', marginBottom: 18 }}>
      <p style={{ fontSize: 12, fontWeight: 700, color: '#7b1c2e', margin: '0 0 6px', textTransform: 'uppercase', letterSpacing: '0.06em' }}>
        Outputs you will get
      </p>
      <ul style={{ margin: 0, paddingLeft: 18 }}>
        {outputs.map((o, i) => (
          <li key={i} style={{ fontSize: 13, color: '#555563', marginBottom: 2 }}>{o}</li>
        ))}
      </ul>
    </div>
  );
}
