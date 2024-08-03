package openjoe.smart.sso.base.enums;

/**
 * OAuth2授权方式
 *
 * @author Joe
 */
public enum GrantTypeEnum {

    /**
     * 授权码模式
     */
    AUTHORIZATION_CODE("authorization_code");

    private String value;

    GrantTypeEnum(String value) {
        this.value = value;
    }

    public String getValue() {
        return this.value;
    }
}