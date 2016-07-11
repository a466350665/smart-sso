package com.smart.base.service.impl;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.smart.base.dao.UserDao;
import com.smart.base.model.User;
import com.smart.base.service.UserService;
import com.smart.ssm.model.Pagination;
import com.smart.ssm.service.impl.ServiceImpl;

@Service("userService")
public class UserServiceImpl extends ServiceImpl<UserDao, User, Integer> implements UserService {

	@Autowired
	public void setDao(UserDao dao) {
		this.dao = dao;
	}
	
	@Override
	public Pagination<User> findPaginationByAccount(String account, Pagination<User> p) {
		dao.findByAccount(account, p);
		return p;
	}
}
