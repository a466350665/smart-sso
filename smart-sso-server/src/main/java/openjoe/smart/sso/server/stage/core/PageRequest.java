package openjoe.smart.sso.server.stage.core;

public class PageRequest {

    /**
     * 当前页
     */
    private Long current = 1L;

    /**
     * 每页记录数
     */
    private Long size = 10L;

    public Long getCurrent() {
        return current;
    }

    public void setCurrent(Long current) {
        this.current = current;
    }

    public Long getSize() {
        return size;
    }

    public void setSize(Long size) {
        this.size = size;
    }
}