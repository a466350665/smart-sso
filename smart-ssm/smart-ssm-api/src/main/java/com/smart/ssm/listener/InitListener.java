package com.smart.ssm.listener;

import javax.servlet.ServletContext;
import javax.servlet.ServletContextAttributeEvent;
import javax.servlet.ServletContextAttributeListener;
import javax.servlet.ServletContextEvent;
import javax.servlet.ServletContextListener;
import javax.servlet.http.HttpSessionEvent;
import javax.servlet.http.HttpSessionListener;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.smart.ssm.config.ConfigUtils;

/**
 * 初始化全局参数(备注：引用系统有特殊需求，可自行继承覆盖)
 * 
 * @author Joe
 */
public class InitListener implements HttpSessionListener, ServletContextListener, ServletContextAttributeListener {
	
	private static final Logger LOGGER = LoggerFactory.getLogger(InitListener.class);

	@Override
	public void attributeAdded(ServletContextAttributeEvent arg0) {
	}

	@Override
	public void attributeRemoved(ServletContextAttributeEvent arg0) {
	}

	@Override
	public void attributeReplaced(ServletContextAttributeEvent arg0) {
	}

	@Override
	public void contextDestroyed(ServletContextEvent sce) {
	}

	@Override
	public void contextInitialized(ServletContextEvent event) {
		ServletContext servletContext = (ServletContext) event.getServletContext();
		servletContext.setAttribute("_path", servletContext.getContextPath());
		try {
			servletContext.setAttribute("_staticPath", ConfigUtils.getProperty("static.url"));
			servletContext.setAttribute("_systemName", ConfigUtils.getProperty("system.name"));
			servletContext.setAttribute("_systemAdminName", ConfigUtils.getProperty("system.admin.name"));
			servletContext.setAttribute("_companyName", ConfigUtils.getProperty("system.company.name"));
			servletContext.setAttribute("_companyPhone", ConfigUtils.getProperty("system.company.phone"));
			servletContext.setAttribute("_copyRight", ConfigUtils.getProperty("system.copy.right"));
		}
		catch (Exception e) {
			LOGGER.error("系统初始化参数配置有误", e);
		}
	}

	@Override
	public void sessionCreated(HttpSessionEvent arg0) {
	}

	public void sessionDestroyed(HttpSessionEvent arg0) {
	}
}
