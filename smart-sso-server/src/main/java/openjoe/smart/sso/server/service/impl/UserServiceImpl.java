package openjoe.smart.sso.server.service.impl;

import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import openjoe.smart.sso.base.entity.Result;
import openjoe.smart.sso.base.entity.TokenPermission;
import openjoe.smart.sso.base.entity.TokenUser;
import openjoe.smart.sso.server.entity.App;
import openjoe.smart.sso.server.entity.User;
import openjoe.smart.sso.server.manager.UserManager;
import openjoe.smart.sso.server.mapper.UserMapper;
import openjoe.smart.sso.server.service.*;
import openjoe.smart.sso.server.util.PasswordHelper;
import openjoe.smart.stage.core.entity.Page;
import openjoe.smart.stage.mybatisplus.service.impl.BaseServiceImpl;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.StringUtils;

import java.util.Collection;
import java.util.Collections;
import java.util.Date;
import java.util.List;

@Service("userService")
public class UserServiceImpl extends BaseServiceImpl<UserMapper, User> implements UserService, UserManager {
	
	@Autowired
	private UserRoleService userRoleService;
	@Autowired
	private OfficeService officeService;
	@Autowired
	private PermissionService permissionService;
	@Autowired
	private AppService appService;

	@Override
	public Result<TokenUser> login(String username, String password, String clientId) {
		User user = selectByAccount(username);
		if (user == null) {
			return Result.error("用户不存在");
		}
		else if (!user.getPassword().equals(PasswordHelper.encrypt(password))) {
			return Result.error("密码不正确");
		}
		else if (!user.getIsEnable()) {
			return Result.error("已被用户禁用");
		}
		else {
			user.setLoginCount(user.getLoginCount() + 1);
			user.setLastLoginTime(new Date());
			updateById(user);
		}
		return Result.success(new TokenUser(user.getId(), user.getAccount()));
	}

	@Override
	public TokenPermission getUserPermission(Long userId, String clientId) {
		App app = appService.selectByClientId(clientId);
		if (app == null || !app.getIsEnable()) {
			return new TokenPermission(Collections.emptySet(), Collections.emptySet(), Collections.emptyList());
		}
		return permissionService.getUserPermission(userId, app.getId());
	}

	@Override
    @Transactional
    public void enable(Boolean isEnable, List<Long> idList) {
        selectByIds(idList).forEach(t -> {
            t.setIsEnable(isEnable);
            updateById(t);
        });
    }

	private List<User> selectByIds(List<Long> idList){
		LambdaQueryWrapper<User> wrapper =  Wrappers.lambdaQuery();
		wrapper.in(User::getId, idList);
		return list(wrapper);
	}
	
	@Override
	@Transactional
    public void resetPassword(String password, List<Long> idList) {
        idList.forEach(id -> updatePassword(id, password));
    }

	@Override
	public Page<User> selectPage(String account, String name, Long officeId, Long current, Long size) {
		LambdaQueryWrapper<User> wrapper =  Wrappers.lambdaQuery();
		wrapper.like(!StringUtils.isEmpty(account), User::getAccount, account)
		    .like(!StringUtils.isEmpty(name), User::getName, name).orderByDesc(User::getCreateTime);
		if(officeId!=null) {
			wrapper.in(User::getOfficeId, officeService.selectIdListByParentId(officeId));
		}
		return findPage(current, size, wrapper);
	}
	
	@Override
	public User selectByAccount(String account) {
		LambdaQueryWrapper<User> wrapper =  Wrappers.lambdaQuery();
		wrapper.eq(User::getAccount, account);
		return getOne(wrapper);
	}
	
	@Transactional
	@Override
	public void deleteByIds(Collection<Long> idList) {
		userRoleService.deleteByUserIds(idList);
		super.removeByIds(idList);
	}

	@Override
	public void updatePassword(Long id, String newPassword) {
		User user = getById(id);
		user.setPassword(PasswordHelper.encrypt(newPassword));
		updateById(user);
	}
}
