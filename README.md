# pms-infra-agent-server

----
## ⚠️ Caution

**Do not grant unrestricted control to AI.**

Unsupervised use or misuse may lead to unintended consequences.
All AI systems must remain strictly under human oversight and control.
Use responsibly, with full awareness and at your own risk.

----
## 📘 Overview

**`pms-infra-agent-server`** is a Haskell infrastructure library that enables AI agents to act as a **TCP server**, accepting inbound connections from external clients and exchanging arbitrary data at the byte level.

Unlike client-oriented socket libraries, this library exposes server-side listen, accept, and I/O operations as MCP tools. The agent retains full control over protocol handling — whether Telnet, HTTP, custom binary frames, or AI-to-AI communication protocols.

The library is a core component of the [`pty-mcp-server`](https://github.com/phoityne/pty-mcp-server) ecosystem and implements the `agent-server-*` family of MCP tools.

---

## 🔧 Provided MCP Tools

### `agent-server-listen`
Binds to the specified host and port, starts listening for incoming TCP connections, and launches a background accept thread. Returns immediately after the listener is ready.
Only one listener can be active at a time.

- `host` — Bind address (e.g. `0.0.0.0` for all interfaces, `127.0.0.1` for loopback)
- `port` — Port number to listen on (e.g. `19999`)

### `agent-server-close`
Closes the active accepted connection (if any). If no connection is active, also closes the listener.
Call twice to close both the accepted connection and the listener independently.

### `agent-server-status`
Returns the current server status as a JSON object.

```json
{ "isListening": true, "isConnected": false }
```

- `isListening` — Whether the listener socket is active
- `isConnected` — Whether an accepted connection is currently open

### `agent-server-events`
Dequeues and returns all server-side events accumulated since the last call.
Returns an empty array if no events are pending.

Event types:
- `ClientConnected` — A client has been accepted
- `BytesReceived` — Bytes received from the client (hex-encoded)
- `ClientDisconnected` — The client has closed the connection

```json
[
  { "tag": "ClientConnected", "handleName": "default" },
  { "tag": "BytesReceived", "handleName": "default", "bytes": "68656C6C6F0D0A" },
  { "tag": "ClientDisconnected", "handleName": "default" }
]
```

### `agent-server-read`
Reads data from the active accepted connection and returns it as a **UTF-8 string**.
Returns an empty string if no data is available before timeout.

> ⚠️ If the received data contains non-UTF-8 bytes or binary protocol frames, use `agent-server-read-byte` instead.

### `agent-server-read-byte`
Reads data from the active accepted connection and returns it as an **uppercase hex string** (e.g. `FF0A1B41`).
Use this for binary protocols or when precise byte-level inspection is required.

### `agent-server-write`
Writes the specified **UTF-8 string** to the active accepted connection.

- `data` — Text data to write

> ⚠️ `\r\n` in the string is sent as the two literal characters `\` and `r`, not as CRLF bytes.
> Use `agent-server-write-byte` when correct CRLF (or any exact byte sequence) is required.

### `agent-server-write-byte`
Decodes the specified **hex string** and writes the resulting bytes to the active accepted connection.
Use this for binary protocols or when precise byte-level control is required.

- `data` — Hex string to decode and write (e.g. `48656C6C6F0D0A`; uppercase and lowercase are accepted, no spaces or newlines)

---

## 💡 Usage Notes

### Busy rejection
If a second client attempts to connect while a connection is already active, the server automatically sends `busy\r\n` to the second client and closes it immediately. The existing connection is unaffected.

### CRLF and binary sending
Always use `agent-server-write-byte` when the protocol requires exact CRLF bytes or binary content.
Generate hex strings with:
```bash
python3 -c "print('HELLO\r\n'.encode().hex())"
```
> The hex string passed to `agent-server-write-byte` must not contain spaces or newlines.

### Receiving data
There is no blocking read tool. Data arrives asynchronously via the internal event queue.
Poll `agent-server-events` to retrieve `BytesReceived` events and decode the hex bytes.

### Closing sequence
When initiating a graceful shutdown, send a `BYE\r\n` signal to the client **and wait for the client's `ACK\r\n`** (polled via `agent-server-events`) before calling `agent-server-close`.
Closing immediately after sending `BYE` may cause the client to miss the message.

```
Server → Client : BYE\r\n
Server ← Client : ACK\r\n   ← wait for this via agent-server-events
Server  agent-server-close
```

### AI-to-AI communication protocol
This library ships with two MCP prompt skills for AI-to-AI TCP communication:

| Skill | File | Trigger examples |
|-------|------|------------------|
| Server role | `skill_agent_server.md` | "start the server", "listen on port 19999" |
| Client role | `skill_agent_client.md` | "connect to 172.16.0.43:19999", "connect to the AI server" |

Handshake protocol:
```
Server → Client : HELLO? name?\r\n
Server ← Client : NAME: <name>\r\n
Server → Client : RULES: MSG:<content>\r\n | REPLY:<content>\r\n | BYE\r\n | HEX:<hex>\r\n
Server ← Client : ACK\r\n
--- conversation ---
Server → Client : BYE\r\n
Server ← Client : ACK\r\n
```

---

## 🚀 Usage Examples

### Example 1: Telnet client session

This example shows a full session where a telnet client connects, sends `hello`, and disconnects.

**Step 1 — Start listening**
```
[Agent] agent-server-listen host="0.0.0.0" port=19999
→ "listening."
```

**Step 2 — Client connects (telnet 172.16.0.43 19999)**
```
[Agent] agent-server-events
→ [{ "tag": "ClientConnected", "handleName": "default" }]
```

**Step 3 — Client sends "hello" and disconnects (Ctrl+] → quit)**
```
[Agent] agent-server-events
→ [
    { "tag": "BytesReceived", "handleName": "default", "bytes": "68656C6C6F0D0A" },
    { "tag": "ClientDisconnected", "handleName": "default" }
  ]
```
`68656C6C6F0D0A` decodes to `hello\r\n`.

**Step 4 — Confirm listener is still active after disconnect**
```
[Agent] agent-server-status
→ { "isListening": true, "isConnected": false }
```

**Step 5 — Stop listening**
```
[Agent] agent-server-close
→ "listener closed."

[Agent] agent-server-status
→ { "isListening": false, "isConnected": false }
```

---

### Example 2: Busy rejection

When a second client connects while a connection is already active, the server automatically rejects it.

```
[Agent] agent-server-listen host="0.0.0.0" port=19999
→ "listening."

--- Client A connects: telnet 172.16.0.43 19999 (connection maintained) ---

--- Client B connects: telnet 172.16.0.43 19999 ---
Client B receives: "busy"
Client B sees:     "Connection closed by foreign host."

[Agent] agent-server-status
→ { "isListening": true, "isConnected": true }   ← Client A is still connected

[Agent] agent-server-events
→ []   ← busy rejection does not produce events
```

---

### Example 3: HTTP response via curl

This example shows the agent acting as an HTTP server, receiving a `GET` request from `curl` and responding with an HTML document.

**Step 1 — Start listening**
```
[Agent] agent-server-listen host="0.0.0.0" port=19999
→ "listening."
```

**Step 2 — curl sends an HTTP request**
```bash
$ curl http://172.16.0.43:19999/
```

**Step 3 — Read the HTTP request from the event queue**
```
[Agent] agent-server-events
→ [{
    "tag": "BytesReceived",
    "handleName": "default",
    "bytes": "474554202F20485454502F312E310D0A486F73743A203137322E31362E302E34333A31393939390D0A557365722D4167656E743A206375726C2F382E31322E310D0A4163636570743A202A2F2A0D0A0D0A"
  }]
```
Decoded:
```
GET / HTTP/1.1\r\n
Host: 172.16.0.43:19999\r\n
User-Agent: curl/8.12.1\r\n
Accept: */*\r\n
\r\n
```

**Step 4 — Send the HTTP response using `agent-server-write-byte`**

Generate the hex for the response:
```bash
python3 -c "print((
  'HTTP/1.1 200 OK\r\n'
  'Content-Type: text/html; charset=utf-8\r\n'
  'Content-Length: 46\r\n'
  'Connection: close\r\n'
  '\r\n'
  '<html><body>Hello from AI Agent!</body></html>'
).encode().hex())"
```

```
[Agent] agent-server-write-byte data="<hex string above>"
[Agent] agent-server-close
```

**Result on the curl side:**
```
$ curl http://172.16.0.43:19999/
<html><body>Hello from AI Agent!</body></html>
```

> ⚠️ Always use `agent-server-write-byte` (not `agent-server-write`) for HTTP responses.
> `agent-server-write` sends `\r\n` as two literal characters, not as CRLF bytes,
> causing `curl` to report `Recv failure`.

---


### Module Structure

```
PMS.Infra.Agent.Server
├── CoreModel
│   └── Type          -- Data type definitions (AppData, command types, event types)
├── ProjectedContext
│   └── Core          -- Core domain service logic (recvLoop, sendLoop)
├── ApplicationBase
│   ├── Control       -- Tool dispatch and server lifecycle management
│   └── State
│       ├── Idle       -- State: no listener active
│       ├── Listening  -- State: listener active, no connection
│       └── Connected  -- State: connection accepted
└── Interface
    └── Network       -- TCP socket operations (listen, accept, send, recv)
```

### Key Design Points

- **State machine**: Server lifecycle is modelled as three explicit states — `Idle`, `Listening`, and `Connected`. Each state handles only the events valid for that state.
- **Single active connection**: Only one accepted connection can be active at a time. A second incoming connection receives `busy\r\n` and is rejected.
- **Background accept thread**: `agent-server-listen` returns immediately; a background thread waits for `accept` and pushes a `ClientConnected` event into the queue.
- **Event queue**: All inbound data and connection lifecycle events are pushed to an STM `TQueue`. The agent polls via `agent-server-events`.
- **Socket-based I/O**: All read/write operations use `Network.Socket` directly (no `Handle` abstraction), ensuring predictable close semantics.

---

## 📦 Dependencies

- [`pms-domain-model`](https://github.com/phoityne/pms-domain-model)
- `network`
- `stm`
- `async`

---

## 📜 Credits & License

- **Execution & Process Lead:** Claude Sonnet 4.6
- **Direction & Policy:** phoityne
- **License:** Apache-2.0 — see [LICENSE](./LICENSE)
