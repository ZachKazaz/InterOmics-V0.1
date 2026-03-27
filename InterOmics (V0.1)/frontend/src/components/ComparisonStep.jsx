// src/components/ComparisonStep.jsx — Define Comparison step (DIABLO workflow step 3)
// Displays a group summary table built from session metadata.
// User selects which groups to include; backend uses the "Group" column.

import { useEffect, useState } from 'react';
import { apiGet, apiPost } from '../api/session';
import ErrorAlert from './ErrorAlert';

const C = {
  maroon:      '#7b1c2e',
  maroonLight: '#f5eaec',
  border:      '#e2e2e5',
  textPrimary: '#1a1a24',
  textSec:     '#555563',
  textMuted:   '#888896',
  bg:          '#f4f4f5',
  green:       '#2d6a4f',
  greenLight:  '#d8f3dc',
  warnBg:      '#fff3cd',
  warnBorder:  '#ffc107',
  warnText:    '#856404',
  white:       '#ffffff',
};

export default function ComparisonStep({ sessionId, onComplete }) {
  const [groupTable,     setGroupTable]     = useState(null);
  const [selectedGroups, setSelectedGroups] = useState([]);
  const [loading,        setLoading]        = useState(true);
  const [submitting,     setSubmitting]     = useState(false);
  const [error,          setError]          = useState(null);

  useEffect(() => {
    async function fetchGroupTable() {
      try {
        const data = await apiGet(`/${sessionId}/comparison/candidates`);
        if (data.status === 'error') {
          setError(data);
        } else {
          setGroupTable(data);
          // Default: select all selectable groups
          const defaults = (data.recommended_default ?? []).map(String);
          setSelectedGroups(defaults.length > 0
            ? defaults
            : (data.available_groups ?? [])
                .filter(g => g.selectable)
                .map(g => g.group_id)
          );
        }
      } catch (err) {
        setError({ error_code: 'NETWORK_ERROR', detail: String(err.message ?? err), recoverable: true });
      } finally {
        setLoading(false);
      }
    }
    fetchGroupTable();
  }, [sessionId]);

  function toggleGroup(groupId, selectable) {
    if (!selectable) return;
    setSelectedGroups(prev =>
      prev.includes(groupId)
        ? prev.filter(g => g !== groupId)
        : [...prev, groupId]
    );
  }

  async function handleConfirm() {
    setError(null);
    setSubmitting(true);
    try {
      const data = await apiPost(`/${sessionId}/comparison/define`, {
        selected_groups: selectedGroups,
      });
      if (data.status === 'error') {
        setError(data);
      } else {
        onComplete('preprocess');
      }
    } catch (err) {
      setError({ error_code: 'NETWORK_ERROR', detail: String(err.message ?? err), recoverable: true });
    } finally {
      setSubmitting(false);
    }
  }

  const validSelected = selectedGroups.filter(g => {
    const row = groupTable?.available_groups?.find(r => r.group_id === g);
    return row?.selectable === true;
  });
  const canConfirm = validSelected.length >= 2 && !submitting;

  const descriptorCols = groupTable?.descriptor_columns ?? [];

  return (
    <div>
      <h2 style={headingStyle}>Define Comparison</h2>

      <div style={infoBoxStyle}>
        <p style={infoLabelStyle}>Select groups for DIABLO</p>
        <ul style={{ margin: 0, paddingLeft: 18 }}>
          <li style={liStyle}>Select which metadata-defined groups to include in the DIABLO comparison</li>
          <li style={liStyle}>Each selected group must contain at least 3 samples</li>
          <li style={liStyle}>At least 2 groups are required</li>
        </ul>
      </div>

      <ErrorAlert error={error} onDismiss={() => setError(null)} />

      {loading && (
        <div style={{ color: C.textSec, fontSize: 14, padding: '12px 0' }}>
          Loading group table…
        </div>
      )}

      {!loading && groupTable && (
        <div style={{ maxWidth: 720 }}>

          {/* Group summary table */}
          <table style={tableStyle}>
            <thead>
              <tr>
                <th style={{ ...thStyle, width: 36 }}></th>
                <th style={thStyle}>Group</th>
                <th style={{ ...thStyle, textAlign: 'center' }}>Samples</th>
                {descriptorCols.map(col => (
                  <th key={col} style={thStyle}>{col}</th>
                ))}
              </tr>
            </thead>
            <tbody>
              {(groupTable.available_groups ?? []).map(row => {
                const checked    = selectedGroups.includes(row.group_id);
                const tooFew     = !row.selectable;
                const rowBg      = checked ? '#fdf4f5' : C.white;

                return (
                  <tr
                    key={row.group_id}
                    onClick={() => toggleGroup(row.group_id, row.selectable)}
                    style={{
                      background: rowBg,
                      cursor: tooFew ? 'not-allowed' : 'pointer',
                      opacity: tooFew ? 0.65 : 1,
                      borderBottom: `1px solid ${C.border}`,
                    }}
                  >
                    {/* Checkbox cell */}
                    <td style={{ ...tdStyle, textAlign: 'center', width: 36 }}>
                      <input
                        type="checkbox"
                        checked={checked}
                        disabled={tooFew}
                        onChange={() => toggleGroup(row.group_id, row.selectable)}
                        onClick={e => e.stopPropagation()}
                        style={{ accentColor: C.maroon, cursor: tooFew ? 'not-allowed' : 'pointer' }}
                      />
                    </td>

                    {/* Group name */}
                    <td style={{ ...tdStyle, fontWeight: checked ? 600 : 400, color: C.textPrimary }}>
                      {row.group_id}
                    </td>

                    {/* Sample count + warning badge */}
                    <td style={{ ...tdStyle, textAlign: 'center' }}>
                      {tooFew ? (
                        <span style={warnBadgeStyle}>
                          {row.n_samples} &lt;3
                        </span>
                      ) : (
                        <span style={countBadgeStyle}>{row.n_samples}</span>
                      )}
                    </td>

                    {/* Descriptor columns */}
                    {descriptorCols.map(col => (
                      <td key={col} style={{ ...tdStyle, color: C.textSec }}>
                        {row[col] ?? '—'}
                      </td>
                    ))}
                  </tr>
                );
              })}
            </tbody>
          </table>

          {/* Validation feedback */}
          {validSelected.length === 1 && (
            <div style={warnBoxStyle}>
              Select at least 2 groups to define a valid comparison.
            </div>
          )}
          {validSelected.length === 0 && !loading && (
            <div style={warnBoxStyle}>
              No groups selected. Check at least 2 groups to continue.
            </div>
          )}

          {/* Selection summary */}
          {validSelected.length >= 2 && (
            <div style={summaryStyle}>
              <strong>{validSelected.length} groups</strong> selected:{' '}
              {validSelected.join(', ')}
            </div>
          )}

          <button
            onClick={handleConfirm}
            disabled={!canConfirm}
            style={btnStyle(!canConfirm)}
          >
            {submitting ? 'Saving…' : 'Continue to Preprocessing'}
          </button>
        </div>
      )}
    </div>
  );
}

// ─── Styles ───────────────────────────────────────────────────────────────────
const headingStyle   = { fontSize: 20, fontWeight: 700, color: C.textPrimary, margin: '0 0 14px' };
const infoBoxStyle   = { background: C.maroonLight, border: '1px solid #e2c8cc', borderRadius: 6, padding: '10px 14px', marginBottom: 18, maxWidth: 640, textAlign: 'left' };
const infoLabelStyle = { fontSize: 12, fontWeight: 700, color: C.maroon, margin: '0 0 6px', textTransform: 'uppercase', letterSpacing: '0.06em' };
const liStyle        = { fontSize: 13, color: C.textSec, marginBottom: 3 };
const tableStyle     = { width: '100%', borderCollapse: 'collapse', fontSize: 13, marginBottom: 16, border: `1px solid ${C.border}`, borderRadius: 5, overflow: 'hidden' };
const thStyle        = { padding: '7px 12px', background: C.bg, borderBottom: `1px solid ${C.border}`, textAlign: 'left', fontWeight: 600, fontSize: 12, color: C.textPrimary, whiteSpace: 'nowrap' };
const tdStyle        = { padding: '8px 12px', fontSize: 13 };
const countBadgeStyle = { display: 'inline-block', background: C.bg, border: `1px solid ${C.border}`, borderRadius: 3, padding: '1px 8px', fontSize: 12, color: C.textSec, fontWeight: 500 };
const warnBadgeStyle  = { display: 'inline-block', background: C.warnBg, border: `1px solid ${C.warnBorder}`, borderRadius: 3, padding: '1px 8px', fontSize: 11, color: C.warnText, fontWeight: 600 };
const warnBoxStyle   = { background: C.warnBg, border: `1px solid ${C.warnBorder}`, borderRadius: 4, padding: '8px 12px', marginBottom: 10, color: C.warnText, fontSize: 13 };
const summaryStyle   = { background: C.greenLight, border: '1px solid #b7e4c7', borderRadius: 4, padding: '8px 12px', marginBottom: 14, color: C.green, fontSize: 13 };
const btnStyle = disabled => ({
  padding: '9px 22px',
  background: disabled ? '#bbb' : C.maroon,
  color: '#fff',
  border: 'none',
  borderRadius: 5,
  cursor: disabled ? 'not-allowed' : 'pointer',
  fontWeight: 600,
  fontSize: 14,
  marginTop: 4,
});
