package com.smart.sso.server.dao;

import java.util.List;

import org.apache.ibatis.annotations.Param;

import com.smart.mvc.dao.DynamicDao;
import com.smart.sso.client.dto.RpcPermissionDto;
import com.smart.sso.server.model.Permission;

/**
 * 权限持久化接口
 * 
 * @author Joe
 */
public interface PermissionDao extends DynamicDao<Permission> {

	public List<RpcPermissionDto> selectListByUserId(@Param("appCode") String appCode, @Param("userId") Integer userId);
}
