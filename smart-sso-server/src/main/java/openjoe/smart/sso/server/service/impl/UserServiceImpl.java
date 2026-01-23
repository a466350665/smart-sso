package openjoe.smart.sso.server.service.impl;

import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import openjoe.smart.sso.base.entity.Result;
import openjoe.smart.sso.base.entity.TokenUser;
import openjoe.smart.sso.server.entity.User;
import openjoe.smart.sso.server.manager.UserManager;
import openjoe.smart.sso.server.mapper.UserMapper;
import openjoe.smart.sso.server.service.OfficeService;
import openjoe.smart.sso.server.service.UserRoleService;
import openjoe.smart.sso.server.service.UserService;
import openjoe.smart.stage.core.entity.Page;
import openjoe.smart.stage.mybatisplus.service.impl.BaseServiceImpl;
import openjoe.smart.sso.server.util.PasswordHelper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.CollectionUtils;
import org.springframework.util.StringUtils;

import java.util.*;
import java.util.stream.Collectors;

@Service("userService")
public class UserServiceImpl extends BaseServiceImpl<UserMapper, User> implements UserService, UserManager {

    @Autowired
    private UserRoleService userRoleService;
    @Autowired
    private OfficeService officeService;

    @Override
    public Result<Long> validate(String username, String password) {
        User user = selectByAccount(username);
        if (user == null) {
            return Result.error("用户不存在");
        } else if (!user.getPassword().equals(PasswordHelper.encrypt(password))) {
            return Result.error("密码不正确");
        } else if (!user.getIsEnable()) {
            return Result.error("已被用户禁用");
        } else {
            user.setLoginCount(user.getLoginCount() + 1);
            user.setLastLoginTime(new Date());
            updateById(user);
        }
        return Result.success(user.getId());
    }

    @Override
    public Result<TokenUser> getTokenUser(Long userId) {
        User user = getById(userId);
        if (user == null) {
            return Result.error("用户不存在");
        }
        return Result.success(new TokenUser(user.getId(), user.getAccount()));
    }

    @Override
    @Transactional
    public void enable(Boolean isEnable, List<Long> idList) {
        selectByIds(idList).forEach(t -> {
            t.setIsEnable(isEnable);
            updateById(t);
        });
    }

    private List<User> selectByIds(Collection<Long> idList) {
        LambdaQueryWrapper<User> wrapper = Wrappers.lambdaQuery();
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
        LambdaQueryWrapper<User> wrapper = Wrappers.lambdaQuery();
        wrapper.like(StringUtils.hasLength(account), User::getAccount, account)
                .like(StringUtils.hasLength(name), User::getName, name).orderByDesc(User::getCreateTime);
        if (officeId != null) {
            wrapper.in(User::getOfficeId, officeService.selectIdListByParentId(officeId));
        }
        return findPage(current, size, wrapper);
    }

    @Override
    public User selectByAccount(String account) {
        LambdaQueryWrapper<User> wrapper = Wrappers.lambdaQuery();
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
    public Map<Long, User> selectMapByIds(Collection<Long> idList) {
        List<User> list = selectByIds(idList);
        if (CollectionUtils.isEmpty(list)) {
            return Collections.emptyMap();
        }
        return list.stream().collect(Collectors.toMap(User::getId, t -> t));
    }

    @Override
    public Set<Long> selectUserIds(String account, String name) {
        LambdaQueryWrapper<User> wrapper = Wrappers.lambdaQuery();
        wrapper.like(StringUtils.hasLength(account), User::getAccount, account)
                .like(StringUtils.hasLength(name), User::getName, name).orderByDesc(User::getCreateTime);
        List<User> list = list(wrapper);
        if (CollectionUtils.isEmpty(list)) {
            return Collections.emptySet();
        }
        return list.stream().map(User::getId).collect(Collectors.toSet());
    }

    @Override
    public void updatePassword(Long id, String newPassword) {
        User user = getById(id);
        user.setPassword(PasswordHelper.encrypt(newPassword));
        updateById(user);
    }
}
