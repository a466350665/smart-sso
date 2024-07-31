package openjoe.smart.sso.server.stage.mybatisplus.config;

import org.springframework.boot.context.properties.ConfigurationProperties;

@ConfigurationProperties("smart.stage.mybatis-plus")
public class MybatisPlusProperties {

    private String pageDbType;

    public String getPageDbType() {
        return pageDbType;
    }

    public void setPageDbType(String pageDbType) {
        this.pageDbType = pageDbType;
    }
}