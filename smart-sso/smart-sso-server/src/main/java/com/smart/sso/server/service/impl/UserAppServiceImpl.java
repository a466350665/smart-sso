package com.smart.sso.server.service.impl;

import java.util.Arrays;
import java.util.List;

import javax.annotation.Resource;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.smart.mvc.service.mybatis.impl.ServiceImpl;
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
	public void allocate(Integer userId, List<Integer> idList, List<UserApp> list) {
		userRoleService.deleteForChangeApp(userId, idList);
		dao.deleteByUserIds(Arrays.asList(userId));
		super.save(list);
	}
	
	public UserApp findByUserAppId(Integer userId, Integer roleId) {
		return dao.findByUserAppId(userId, roleId);
	}
	
	public void deleteByUserIds(List<Integer> idList) {
		dao.deleteByUserIds(idList);
	}
	
	public void deleteByAppIds(List<Integer> idList) {
		dao.deleteByAppIds(idList);
	}
}
