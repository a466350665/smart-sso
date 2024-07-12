package openjoe.smart.sso.base.entity;

/**
 * 用户信息
 *
 * @author Joe
 */
public class TokenUser {

    /**
     * 用户ID
     */
    private Integer id;
    /**
     * 用户名
     */
    private String username;

    public TokenUser() {
    }

    public TokenUser(Integer id, String username) {
        super();
        this.id = id;
        this.username = username;
    }

    public Integer getId() {
        return id;
    }

    public void setId(Integer id) {
        this.id = id;
    }

    public String getUsername() {
        return username;
    }

    public void setUsername(String username) {
        this.username = username;
    }
}
