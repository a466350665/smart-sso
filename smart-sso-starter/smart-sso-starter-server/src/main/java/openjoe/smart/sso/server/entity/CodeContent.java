package openjoe.smart.sso.server.entity;

/**
 * 授权码存储信息
 *
 * @author Joe
 */
public class CodeContent {

    private String tgt;
    private String clientId;

    public CodeContent() {
    }

    public CodeContent(String tgt, String clientId) {
        this.tgt = tgt;
        this.clientId = clientId;
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
}