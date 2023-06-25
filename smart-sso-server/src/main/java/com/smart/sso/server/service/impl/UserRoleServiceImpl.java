package com.smart.sso.server.service.impl;

import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import com.google.common.collect.Lists;
import com.smart.sso.server.service.impl.BaseServiceImpl;
import com.smart.sso.server.dao.UserRoleDao;
import com.smart.sso.server.model.UserRole;
import com.smart.sso.server.service.UserRoleService;
import com.smart.sso.server.util.ConvertUtils;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.Arrays;
import java.util.Collection;
import java.util.List;

@Service("userRoleService")
public class UserRoleServiceImpl extends BaseServiceImpl<UserRoleDao, UserRole> implements UserRoleService {

    @Transactional
    @Override
    public void allocate(Integer userId, List<Integer> roleIdList) {
        deleteByUserIds(Arrays.asList(userId));
        saveBatch(createUserRoleList(userId, roleIdList));
    }
    
    private List<UserRole> createUserRoleList(Integer userId, List<Integer> roleIdList) {
        List<UserRole> userRoleList = Lists.newArrayList();
        UserRole bean;
        for (Integer roleId : roleIdList) {
            bean = new UserRole();
            bean.setUserId(userId);
            bean.setRoleId(roleId);
            userRoleList.add(bean);
        }
        return userRoleList;
    }
	
	@Override
	public UserRole selectByUserRoleId(Integer userId, Integer roleId) {
        LambdaQueryWrapper<UserRole> wrapper =  Wrappers.lambdaQuery();
        wrapper.eq(UserRole::getUserId, userId);
        wrapper.eq(UserRole::getRoleId, roleId);
        return getOne(wrapper);
	}
	
	@Override
	public void deleteByRoleIds(Collection<Integer> idList) {
        LambdaQueryWrapper<UserRole> wrapper =  Wrappers.lambdaQuery();
        wrapper.in(UserRole::getRoleId, idList);
        remove(wrapper);
	}
	
	@Override
	public void deleteByUserIds(Collection<Integer> idList) {
        LambdaQueryWrapper<UserRole> wrapper =  Wrappers.lambdaQuery();
        wrapper.in(UserRole::getUserId, idList);
        remove(wrapper);
	}
	
	@Override
    public List<Integer> findRoleIdListByUserId(Integer userId) {
        return ConvertUtils.convert(findByUserId(userId), pu -> pu.getRoleId());
    }
	
	private List<UserRole> findByUserId(Integer userId) {
        LambdaQueryWrapper<UserRole> wrapper =  Wrappers.lambdaQuery();
        wrapper.eq(UserRole::getUserId, userId);
        return list(wrapper);
    }
}
