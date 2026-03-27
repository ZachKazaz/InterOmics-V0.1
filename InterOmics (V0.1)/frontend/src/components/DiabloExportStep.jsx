import { useState, useEffect } from 'react';
import { apiGet } from '../api/session';

export default function DiabloExportStep({ sessionId }) {
  const [summary, setSummary] = useState(null);
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState(null);

  useEffect(function () {
    if (!sessionId) return;
    apiGet('/' + sessionId + '/analysis/diablo/results')
      .then(function (data) {
        if (data.status === 'error' || data.error_code) {
          setError(data.detail || 'Could not load DIABLO results.');
        } else {
          setSummary(data);
        }
      })
      .catch(function (e) { setError(String(e.message || e)); })
      .finally(function () { setLoading(false); });
  }, [sessionId]);

  if (loading) return <p style={{ fontSize: 13, color: '#888896' }}>Loading export options...</p>;
  if (error)   return <p style={{ fontSize: 13, color: '#c0392b' }}>Error: {error}</p>;
  if (!summary) return null;

  return (
    <div style={{ maxWidth: 700 }}>
      <h2 style={{ fontSize: 20, fontWeight: 700, color: '#1a1a24', margin: '0 0 16px' }}>
        Export DIABLO Results
      </h2>
      <p style={{ fontSize: 13, color: '#888896' }}>
        Download DIABLO analysis outputs from your current session.
      </p>
    </div>
  );
}