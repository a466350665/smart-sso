package com.smart.sso.server.service.impl;

import java.util.Arrays;
import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.smart.ssm.service.impl.ServiceImpl;
import com.smart.sso.server.dao.UserRoleDao;
import com.smart.sso.server.model.UserRole;
import com.smart.sso.server.service.UserRoleService;

@Service("userRoleService")
public class UserRoleServiceImpl extends ServiceImpl<UserRoleDao, UserRole, Integer> implements UserRoleService {

	@Autowired
	public void setDao(UserRoleDao dao) {
		this.dao = dao;
	}
	
	@Transactional
	public int allocate(Integer userId, Integer appId, List<UserRole> list) {
		dao.deleteByUserIds(Arrays.asList(userId), appId);
		return super.save(list);
	}
	
	@Override
	public UserRole findByUserRoleId(Integer userId, Integer roleId) {
		return dao.findByUserRoleId(userId, roleId);
	}
	
	@Override
	public int deleteByRoleIds(List<Integer> idList) {
		return dao.deleteByRoleIds(idList);
	}
	
	@Override
	public int deleteByUserIds(List<Integer> idList, Integer appId) {
		return dao.deleteByUserIds(idList, appId);
	}
	
	@Override
	public int deleteByAppIds(List<Integer> idList) {
		return dao.deleteByAppIds(idList);
	}

	@Override
	public int deleteForChangeApp(Integer userId, List<Integer> idList) {
		return dao.deleteForChangeApp(userId, idList);
	}
}
