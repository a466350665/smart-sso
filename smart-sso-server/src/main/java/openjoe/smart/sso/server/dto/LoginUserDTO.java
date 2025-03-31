package openjoe.smart.sso.server.dto;

import java.util.Date;

public class LoginUserDTO {

    // 用户ID
    private Long id;
    /** 姓名 */
    private String name;
    /** 登录名 */
    private String account;
    /** 登录凭证 */
    private String tgt;
    /** 凭证创建时间 */
    private Date createTime;
    /** 已访问应用 */
    private String apps;

    public Long getId() {
        return id;
    }

    public void setId(Long id) {
        this.id = id;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public String getAccount() {
        return account;
    }

    public void setAccount(String account) {
        this.account = account;
    }

    public String getTgt() {
        return tgt;
    }

    public void setTgt(String tgt) {
        this.tgt = tgt;
    }

    public Date getCreateTime() {
        return createTime;
    }

    public void setCreateTime(Date createTime) {
        this.createTime = createTime;
    }

    public String getApps() {
        return apps;
    }

    public void setApps(String apps) {
        this.apps = apps;
    }
}