package com.smart.sso.server.service.impl;

import java.util.Arrays;
import java.util.List;

import javax.annotation.Resource;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.smart.ssm.service.impl.ServiceImpl;
import com.smart.sso.server.dao.UserAppDao;
import com.smart.sso.server.model.UserApp;
import com.smart.sso.server.service.UserAppService;
import com.smart.sso.server.service.UserRoleService;

@Service("userAppService")
public class UserAppServiceImpl extends ServiceImpl<UserAppDao, UserApp, Integer> implements UserAppService {

	@Resource
	private UserRoleService userRoleService;
	
	@Autowired
	public void setDao(UserAppDao dao) {
		this.dao = dao;
	}
	
	@Transactional
	public int allocate(Integer userId, List<Integer> idList, List<UserApp> list) {
		userRoleService.deleteForChangeApp(userId, idList);
		dao.deleteByUserIds(Arrays.asList(userId));
		return super.save(list);
	}
	
	@Override
	public UserApp findByUserAppId(Integer userId, Integer roleId) {
		return dao.findByUserAppId(userId, roleId);
	}
	
	@Override
	public int deleteByUserIds(List<Integer> idList) {
		return dao.deleteByUserIds(idList);
	}
	
	@Override
	public int deleteByAppIds(List<Integer> idList) {
		return dao.deleteByAppIds(idList);
	}
}
