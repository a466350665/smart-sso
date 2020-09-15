package com.smart.sso.server;

import javax.servlet.ServletContext;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.ApplicationListener;
import org.springframework.context.event.ContextRefreshedEvent;
import org.springframework.web.context.WebApplicationContext;

public class ConfigInitListener implements ApplicationListener<ContextRefreshedEvent> {

    protected Logger logger = LoggerFactory.getLogger(getClass());

    @Value("${system.name}")
    private String systemName;
    @Value("${system.admin.name}")
    private String systemAdminName;
    @Value("${system.company.name}")
    private String companyName;
    @Value("${system.company.phone}")
    private String companyPhone;
    @Value("${system.copy.right}")
    private String copyRight;

    @Override
    public void onApplicationEvent(ContextRefreshedEvent contextRefreshedEvent) {
        // 将 ApplicationContext 转化为 WebApplicationContext
        WebApplicationContext webApplicationContext =
            (WebApplicationContext)contextRefreshedEvent.getApplicationContext();
        // 从 webApplicationContext 中获取 servletContext
        ServletContext servletContext = webApplicationContext.getServletContext();
        // servletContext设置值
        servletContext.setAttribute("_path", servletContext.getContextPath());
        try {
            servletContext.setAttribute("_staticPath", servletContext.getContextPath());
            servletContext.setAttribute("_systemName", systemName);
            servletContext.setAttribute("_systemAdminName", systemAdminName);
            servletContext.setAttribute("_companyName", companyName);
            servletContext.setAttribute("_companyPhone", companyPhone);
            servletContext.setAttribute("_copyRight", copyRight);
        } catch (Exception e) {
            logger.error("系统初始化参数配置有误", e);
        }
    }
}