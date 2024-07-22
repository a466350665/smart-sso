package openjoe.smart.sso.server.entity;

/**
 * 授权码存储信息
 *
 * @author Joe
 */
public class CodeContent {

    private String tgt;
    private String clientId;
    private String redirectUri;

    public CodeContent() {
    }

    public CodeContent(String tgt, String clientId, String redirectUri) {
        this.tgt = tgt;
        this.clientId = clientId;
        this.redirectUri = redirectUri;
    }

    public String getTgt() {
        return tgt;
    }

    public void setTgt(String tgt) {
        this.tgt = tgt;
    }

    public String getClientId() {
        return clientId;
    }

    public void setClientId(String clientId) {
        this.clientId = clientId;
    }

    public String getRedirectUri() {
        return redirectUri;
    }

    public void setRedirectUri(String redirectUri) {
        this.redirectUri = redirectUri;
    }
}