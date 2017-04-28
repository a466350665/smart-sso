package com.smart.mvc.config;

import java.util.Properties;

import org.springframework.beans.BeansException;
import org.springframework.beans.factory.config.ConfigurableListableBeanFactory;
import org.springframework.beans.factory.config.PropertyPlaceholderConfigurer;

/**
 * 应用配置工具类
 * 
 * @author Joe
 */
public class ConfigUtils extends PropertyPlaceholderConfigurer {

	private static Properties properties;

	@Override
	protected void processProperties(ConfigurableListableBeanFactory beanFactory, Properties props)
			throws BeansException {
		super.processProperties(beanFactory, props);
		properties = props;
	}

	public static String getProperty(String name) {
		return properties.getProperty(name);
	}
}