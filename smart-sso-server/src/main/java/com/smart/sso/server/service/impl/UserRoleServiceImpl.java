package com.smart.sso.server.service.impl;

import java.util.Arrays;
import java.util.Collection;
import java.util.List;

import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.google.common.collect.Lists;
import com.smart.mvc.model.Condition;
import com.smart.mvc.service.impl.ServiceImpl;
import com.smart.mvc.util.ConvertUtils;
import com.smart.sso.server.dao.UserRoleDao;
import com.smart.sso.server.model.UserRole;
import com.smart.sso.server.service.UserRoleService;

@Service("userRoleService")
public class UserRoleServiceImpl extends ServiceImpl<UserRoleDao, UserRole> implements UserRoleService {

    @Transactional
    @Override
    public void allocate(Integer userId, List<Integer> roleIdList) {
        deleteByUserIds(Arrays.asList(userId));
        save(createUserRoleList(userId, roleIdList));
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
		return selectOne(Condition.create().eq("user_id", userId).eq("role_id", roleId));
	}
	
	@Override
	public void deleteByRoleIds(Collection<Integer> idList) {
		deleteByCondition(Condition.create().in("role_id", idList));
	}
	
	@Override
	public void deleteByUserIds(Collection<Integer> idList) {
		deleteByCondition(Condition.create().in("user_id", idList));
	}
	
	@Override
    public List<Integer> findRoleIdListByUserId(Integer userId) {
        return ConvertUtils.convert(findByUserId(userId), pu -> pu.getRoleId());
    }
	
	private List<UserRole> findByUserId(Integer userId) {
        return selectList(Condition.create().eq("user_id", userId));
    }
}
