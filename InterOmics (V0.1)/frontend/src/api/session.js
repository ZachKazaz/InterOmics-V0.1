// src/api/session.js — Session management and API base helpers

const API_BASE = '/api/v1';
const SESSION_KEY = 'session_id';

// Safe JSON parser — never throws on empty or malformed responses
async function safeJson(res) {
  const text = await res.text();
  if (!text || !text.trim()) {
    return {
      status: 'error',
      error_code: 'EMPTY_RESPONSE',
      detail: `Server returned an empty response (HTTP ${res.status}).`,
      recoverable: true,
    };
  }
  try {
    return JSON.parse(text);
  } catch {
    return {
      status: 'error',
      error_code: 'INVALID_JSON',
      detail: `Server returned invalid JSON (HTTP ${res.status}): ${text.slice(0, 120)}`,
      recoverable: true,
    };
  }
}

export async function createSession() {
  const res = await fetch(`${API_BASE}/session`, { method: 'POST' });
  const data = await safeJson(res);
  if (data.status === 'error') throw data;
  sessionStorage.setItem(SESSION_KEY, data.session_id);
  return data.session_id;
}

export function getStoredSessionId() {
  return sessionStorage.getItem(SESSION_KEY);
}

export function clearSession() {
  sessionStorage.removeItem(SESSION_KEY);
}

export async function getSessionState(session_id) {
  const res = await fetch(`${API_BASE}/${session_id}/state`);
  const data = await safeJson(res);
  if (data.status === 'error') throw data;
  return data;
}

export async function apiPost(path, body) {
  const res = await fetch(`${API_BASE}${path}`, {
    method: 'POST',
    headers: { 'Content-Type': 'application/json' },
    body: JSON.stringify(body),
  });
  return safeJson(res);
}

export async function apiPostForm(path, formData) {
  const res = await fetch(`${API_BASE}${path}`, {
    method: 'POST',
    body: formData,
  });
  return safeJson(res);
}

export async function apiGet(path) {
  const res = await fetch(`${API_BASE}${path}`);
  return safeJson(res);
}
