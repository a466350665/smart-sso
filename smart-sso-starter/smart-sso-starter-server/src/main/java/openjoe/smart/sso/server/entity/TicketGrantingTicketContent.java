package openjoe.smart.sso.server.entity;

/**
 * 登录凭证（TGT）存储信息
 *
 * @author Joe
 */
public class TicketGrantingTicketContent {

    private Long userId;
    private Long createTime;

    public TicketGrantingTicketContent() {
    }

    public TicketGrantingTicketContent(Long userId, Long createTime) {
        this.userId = userId;
        this.createTime = createTime;
    }

    public Long getUserId() {
        return userId;
    }

    public void setUserId(Long userId) {
        this.userId = userId;
    }

    public Long getCreateTime() {
        return createTime;
    }

    public void setCreateTime(Long createTime) {
        this.createTime = createTime;
    }
}