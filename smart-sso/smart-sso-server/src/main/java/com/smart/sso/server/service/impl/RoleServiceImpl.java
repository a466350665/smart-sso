package com.smart.sso.server.service.impl;

import java.util.ArrayList;
import java.util.List;

import javax.annotation.Resource;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.smart.mvc.exception.ServiceException;
import com.smart.mvc.model.Pagination;
import com.smart.mvc.service.mybatis.impl.ServiceImpl;
import com.smart.sso.server.common.Permissible;
import com.smart.sso.server.dao.RoleDao;
import com.smart.sso.server.model.Role;
import com.smart.sso.server.service.RolePermissionService;
import com.smart.sso.server.service.RoleService;
import com.smart.sso.server.service.UserRoleService;

@Service("roleService")
public class RoleServiceImpl extends ServiceImpl<RoleDao, Role, Integer> implements RoleService {
	
	@Resource
	private UserRoleService userRoleService;
	@Resource
	private RolePermissionService rolePermissionService;

	@Autowired
	public void setDao(RoleDao dao) {
		this.dao = dao;
	}
	
	@Permissible
	public void enable(Boolean isEnable, List<Integer> idList) {
		int rows = dao.enable(isEnable, idList);
		if (rows != idList.size())
			throw new ServiceException("启用/禁用有误");
	}
	
	@Permissible
	public int saveOrUpdate(Role t) {
		return super.saveOrUpdate(t);
	}

	public Pagination<Role> findPaginationByName(String name, Integer appId, Pagination<Role> p) {
		dao.findPaginationByName(name, null, appId, p);
		return p;
	}
	
	public List<Role> findByAppId(Boolean isEnable, Integer appId) {
		if (appId == null)
			return new ArrayList<Role>(0);
		return dao.findPaginationByName(null, isEnable, appId, null);
	}
	
	@Permissible
	@Transactional
	public int deleteById(List<Integer> idList) {
		userRoleService.deleteByRoleIds(idList);
		rolePermissionService.deleteByRoleIds(idList);
		int rows = dao.deleteById(idList);
		if (rows != idList.size())
			throw new ServiceException("权限删除有误");
		return rows;
	}
	
	public int deleteByAppIds(List<Integer> idList) {
		return dao.deleteByAppIds(idList);
	}
}
