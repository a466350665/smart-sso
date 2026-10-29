# Why OAuth 2.0?

**English** | [中文](why-oauth2.md)

> This article was originally a section of the README and was moved here because of its length. It records the protocol
> trade-offs behind Smart-SSO.

Three common approaches are used for SSO: traditional tokens, JWT, and OAuth 2.0. The table compares them against the
concerns that matter most for SSO:

| Feature | Traditional token | JWT | OAuth 2.0 |
| --- | --- | --- | --- |
| Single sign-on | Supported | Supported | Supported |
| Single logout | Supported | Difficult | Supported |
| Forced logout | Supported | Difficult | Supported |
| Token renewal | Difficult | Supported | Supported |
| Performance | Average | High | Good |
| Security | Average | Good | High |
| Complexity | Average | Higher | High |

## Traditional tokens

The mechanism is simple: the server generates a random string and passes it between the client and server to identify
the user.

The limitations are significant. Without a built-in lifetime and refresh mechanism, silent renewal is difficult. Client
requests also need frequent round trips to the server to validate the token. For a small project, especially one without
high performance or security requirements, this approach may still be sufficient.

## JWT

JWT is stateless: the server stores only the signing key and does not need to store token records, reducing server-side
storage pressure.

In an SSO system, however, **single logout** and **forced logout** are difficult with JWT. These operations usually need
server-side state, remote logout callbacks, or shared storage, which conflicts with the goal of a stateless token. For
systems with stronger security requirements, both operations are often essential.

## OAuth 2.0

OAuth 2.0 is commonly used for delegated login by third-party applications and also fits the SSO scenario. The trade-off
is higher implementation complexity:

- It has a native token **lifetime and refresh mechanism**, enabling renewal. JWT needs a dual-token design to provide
  the same behavior.
- Every application integrating with the authorization server must be **registered and issued credentials** (`ClientId`
  and `ClientSecret`). Tokens can only be obtained through the protocol, providing separate validation of the **user
  identity** when issuing the authorization code and the **client identity** when exchanging it for an access token.
- After a successful login, the authorization server must issue a token for each user and client application separately.
  A token issued to one client must not grant access to resources managed for another client. This matches the intended
  separation in OAuth 2.0.

## Conclusion

Smart-SSO uses OAuth 2.0 and adds focused improvements around its operational trade-offs:

- **Fewer round trips**: the client backend caches tokens, so requests carrying a token can be validated locally,
  greatly reducing client-to-server calls.
- **Automatic renewal**: after the local access token expires, the client backend calls the server with the refresh
  token, obtains a new token, writes it back to the front end, and extends the server-side credential stub.
- **Controlled session lifecycle**: the server owns the global `TGT` session, making single logout and forced logout
  deterministic operations that can be triggered at any time.

See the [architecture and protocol guide](architecture.en.md) for the implementation details.
