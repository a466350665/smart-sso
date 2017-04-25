package com.smart.sso.server.service.impl;

import javax.annotation.Resource;

import org.aspectj.lang.JoinPoint;
import org.aspectj.lang.annotation.AfterReturning;
import org.aspectj.lang.annotation.Aspect;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Component;

import com.smart.sso.server.common.PermissionSubject;
import com.smart.sso.server.service.AppService;

/**
 * 应用权限修改监控切面
 * 
 * 注：@Permissible注解实现
 * 
 * @author Joe
 */
@Component
@Aspect
public class PermissionAspect {

	private static final Logger LOGGER = LoggerFactory.getLogger(PermissionAspect.class);

	@Resource
	private AppService appService;

	@Resource
	private PermissionSubject permissionSubject;

	@AfterReturning(pointcut = "@annotation(com.smart.sso.server.common.Permissible)")
	public void access(JoinPoint jp) {
		try {
			permissionSubject.update();
		}
		catch (Exception e) {
			LOGGER.error("应用权限修改AOP异常", e);
		}
	}
}