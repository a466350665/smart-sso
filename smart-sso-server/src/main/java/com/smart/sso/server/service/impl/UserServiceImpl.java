package com.smart.sso.server.service.impl;

import java.util.Collection;
import java.util.Date;
import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.StringUtils;

import com.smart.mvc.constant.ResultConstant;
import com.smart.mvc.model.Condition;
import com.smart.mvc.model.Page;
import com.smart.mvc.model.Result;
import com.smart.mvc.service.impl.ServiceImpl;
import com.smart.sso.server.dao.UserDao;
import com.smart.sso.server.enums.TrueFalseEnum;
import com.smart.sso.server.model.User;
import com.smart.sso.server.service.OfficeService;
import com.smart.sso.server.service.UserRoleService;
import com.smart.sso.server.service.UserService;
import com.smart.sso.server.util.PasswordHelper;

@Service("userService")
public class UserServiceImpl extends ServiceImpl<UserDao, User> implements UserService {
	
	@Autowired
	private UserRoleService userRoleService;
	@Autowired
	private OfficeService officeService;

	@Override
	public Result<User> login(String account, String password) {
		User user = selectByAccount(account);
		if (user == null) {
		    return Result.create(ResultConstant.SERVICE_ERROR, "用户不存在");
		}
		else if (!user.getPassword().equals(password)) {
		    return Result.create(ResultConstant.SERVICE_ERROR, "密码不正确");
		}
		else if (TrueFalseEnum.FALSE.getValue().equals(user.getIsEnable())) {
		    return Result.create(ResultConstant.SERVICE_ERROR, "已被用户禁用");
		}
		else {
			user.setLoginCount(user.getLoginCount() + 1);
			user.setLastLoginTime(new Date());
			update(user);
		}
		return Result.createSuccess(user);
	}

	@Override
    @Transactional
    public void enable(Boolean isEnable, List<Integer> idList) {
        selectByIds(idList).forEach(t -> {
            t.setIsEnable(isEnable);
            update(t);
        });
    }
	
	@Override
	@Transactional
    public void resetPassword(String password, List<Integer> idList) {
        idList.forEach(id -> updatePassword(id, password));
    }

	@Override
	public Page<User> selectPage(String account, String name, Integer officeId, Page<User> p) {
		Condition c = Condition.create().like(!StringUtils.isEmpty(account), "account", account)
		    .like(!StringUtils.isEmpty(name), "name", name).orderBy("create_time DESC");
		if(officeId!=null) {
		    c.in("office_id", officeService.selectIdListByParentId(officeId));
		}
		return selectPage(c, p);
	}
	
	@Override
	public User selectByAccount(String account) {
		return selectOne(Condition.create().eq("account", account));
	}
	
	@Transactional
	@Override
	public void deleteByIds(Collection<Integer> idList) {
		userRoleService.deleteByUserIds(idList);
		super.deleteByIds(idList);
	}

	@Override
	public void updatePassword(Integer id, String newPassword) {
		User user = get(id);
		user.setPassword(PasswordHelper.encrypt(newPassword));
		update(user);
	}
}
