package com.smart.sso.server.service.impl;

import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import com.smart.sso.server.model.Page;
import com.smart.core.entity.Result;
import com.smart.core.enums.ResultEnum;
import com.smart.sso.server.service.impl.BaseServiceImpl;
import com.smart.sso.server.dao.UserDao;
import com.smart.sso.server.enums.TrueFalseEnum;
import com.smart.sso.server.model.User;
import com.smart.sso.server.service.OfficeService;
import com.smart.sso.server.service.UserRoleService;
import com.smart.sso.server.service.UserService;
import com.smart.sso.server.util.PasswordHelper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.StringUtils;

import java.util.Collection;
import java.util.Date;
import java.util.List;

@Service("userService")
public class UserServiceImpl extends BaseServiceImpl<UserDao, User> implements UserService {
	
	@Autowired
	private UserRoleService userRoleService;
	@Autowired
	private OfficeService officeService;

	@Override
	public Result<User> login(String account, String password) {
		User user = selectByAccount(account);
		if (user == null) {
		    return Result.create(ResultEnum.ERROR.getCode(), "用户不存在");
		}
		else if (!user.getPassword().equals(password)) {
		    return Result.create(ResultEnum.ERROR.getCode(), "密码不正确");
		}
		else if (TrueFalseEnum.FALSE.getValue().equals(user.getIsEnable())) {
		    return Result.create(ResultEnum.ERROR.getCode(), "已被用户禁用");
		}
		else {
			user.setLoginCount(user.getLoginCount() + 1);
			user.setLastLoginTime(new Date());
			updateById(user);
		}
		return Result.createSuccess(user);
	}

	@Override
    @Transactional
    public void enable(Boolean isEnable, List<Integer> idList) {
        selectByIds(idList).forEach(t -> {
            t.setIsEnable(isEnable);
            updateById(t);
        });
    }

	private List<User> selectByIds(List<Integer> idList){
		LambdaQueryWrapper<User> wrapper =  Wrappers.lambdaQuery();
		wrapper.in(User::getId, idList);
		return list(wrapper);
	}
	
	@Override
	@Transactional
    public void resetPassword(String password, List<Integer> idList) {
        idList.forEach(id -> updatePassword(id, password));
    }

	@Override
	public Page<User> selectPage(String account, String name, Integer officeId, Integer pageNo, Integer pageSize) {
		LambdaQueryWrapper<User> wrapper =  Wrappers.lambdaQuery();
		wrapper.like(!StringUtils.isEmpty(account), User::getAccount, account)
		    .like(!StringUtils.isEmpty(name), User::getName, name).orderByDesc(User::getCreateTime);
		if(officeId!=null) {
			wrapper.in(User::getOfficeId, officeService.selectIdListByParentId(officeId));
		}
		return findPage(pageNo, pageSize, wrapper);
	}
	
	@Override
	public User selectByAccount(String account) {
		LambdaQueryWrapper<User> wrapper =  Wrappers.lambdaQuery();
		wrapper.eq(User::getAccount, account);
		return getOne(wrapper);
	}
	
	@Transactional
	@Override
	public void deleteByIds(Collection<Integer> idList) {
		userRoleService.deleteByUserIds(idList);
		super.removeByIds(idList);
	}

	@Override
	public void updatePassword(Integer id, String newPassword) {
		User user = getById(id);
		user.setPassword(PasswordHelper.encrypt(newPassword));
		updateById(user);
	}
}
