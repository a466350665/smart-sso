package com.smart.sso.server.service.impl;

import java.util.Date;
import java.util.List;
import java.util.Set;

import javax.annotation.Resource;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.CollectionUtils;

import com.smart.ssm.enums.TrueFalseEnum;
import com.smart.ssm.exception.ServiceException;
import com.smart.ssm.model.Pagination;
import com.smart.ssm.service.impl.ServiceImpl;
import com.smart.sso.server.common.Permissible;
import com.smart.sso.server.common.Result;
import com.smart.sso.server.dao.UserDao;
import com.smart.sso.server.model.User;
import com.smart.sso.server.service.AppService;
import com.smart.sso.server.service.UserAppService;
import com.smart.sso.server.service.UserRoleService;
import com.smart.sso.server.service.UserService;

@Service("userService")
public class UserServiceImpl extends ServiceImpl<UserDao, User, Integer> implements UserService {
	
	@Resource
	private UserAppService userAppService;
	@Resource
	private UserRoleService userRoleService;
	@Resource
	private AppService appService;

	@Autowired
	public void setDao(UserDao dao) {
		this.dao = dao;
	}
	
	public Result<User> login(String ip, String appCode, String account, String password) {
		Result<User> result = new Result<User>();
		User user = findByAccount(account);
		if (user == null) {
			result.setStatus(Result.ERROR);
			result.setMessage("登录名不存在");
		}
		else if (!user.getPassword().equals(password)) {
			result.setStatus(Result.ERROR);
			result.setMessage("密码不正确");
		}
		else if (TrueFalseEnum.FALSE.getValue().equals(user.getIsEnable())) {
			result.setStatus(Result.ERROR);
			result.setMessage("已被管理员禁用");
		}
		else {
			Set<String> set = appService.findAppCodeByUserId(TrueFalseEnum.TRUE.getValue(), user.getId());
			if (CollectionUtils.isEmpty(set)) {
				result.setStatus(Result.ERROR);
				result.setMessage("不存在可操作应用");
			}
			else if (!set.contains(appCode)) {
				result.setStatus(Result.ERROR);
				result.setMessage("没有应用操作权限");
			}
			else {
				user.setLastLoginIp(ip);
				user.setLoginCount(user.getLoginCount() + 1);
				user.setLastLoginTime(new Date());
				dao.update(user);
				result.setData(user);
			}
		}
		return result;
	}

	@Permissible
	public void enable(Boolean isEnable, List<Integer> idList) {
		int rows = dao.enable(isEnable, idList);
		if (rows != idList.size())
			throw new ServiceException("启用/禁用有误");
	}
	
	@Permissible
	public int saveOrUpdate(User t) {
		return super.saveOrUpdate(t);
	}

	public void resetPassword(String password, List<Integer> idList) {
		int rows = dao.resetPassword(password, idList);
		if (rows != idList.size())
			throw new ServiceException("密码重置有误");
	}

	public Pagination<User> findPaginationByAccount(String account, Integer appId, Pagination<User> p) {
		dao.findPaginationByAccount(account, appId, p);
		return p;
	}
	
	public User findByAccount(String account) {
		return dao.findByAccount(account);
	}
	
	@Permissible
	@Transactional
	public int deleteById(List<Integer> idList) {
		userAppService.deleteByUserIds(idList);
		userRoleService.deleteByUserIds(idList, null);
		int rows = dao.deleteById(idList);
		if (rows != idList.size())
			throw new ServiceException("管理员删除有误");
		return rows;
	}
}
