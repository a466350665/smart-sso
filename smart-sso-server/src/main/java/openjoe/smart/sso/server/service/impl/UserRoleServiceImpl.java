package openjoe.smart.sso.server.service.impl;

import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import com.google.common.collect.Lists;
import openjoe.smart.sso.server.entity.UserRole;
import openjoe.smart.sso.server.mapper.UserRoleDao;
import openjoe.smart.sso.server.service.UserRoleService;
import openjoe.smart.sso.server.util.ConvertUtils;
import openjoe.smart.stage.mybatisplus.service.impl.BaseServiceImpl;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.Arrays;
import java.util.Collection;
import java.util.List;

@Service("userRoleService")
public class UserRoleServiceImpl extends BaseServiceImpl<UserRoleDao, UserRole> implements UserRoleService {

    @Transactional
    @Override
    public void allocate(Long userId, List<Long> roleIdList) {
        deleteByUserIds(Arrays.asList(userId));
        saveBatch(createUserRoleList(userId, roleIdList));
    }
    
    private List<UserRole> createUserRoleList(Long userId, List<Long> roleIdList) {
        List<UserRole> userRoleList = Lists.newArrayList();
        UserRole bean;
        for (Long roleId : roleIdList) {
            bean = new UserRole();
            bean.setUserId(userId);
            bean.setRoleId(roleId);
            userRoleList.add(bean);
        }
        return userRoleList;
    }
	
	@Override
	public UserRole selectByUserRoleId(Long userId, Long roleId) {
        LambdaQueryWrapper<UserRole> wrapper =  Wrappers.lambdaQuery();
        wrapper.eq(UserRole::getUserId, userId);
        wrapper.eq(UserRole::getRoleId, roleId);
        return getOne(wrapper);
	}
	
	@Override
	public void deleteByRoleIds(Collection<Long> idList) {
        LambdaQueryWrapper<UserRole> wrapper =  Wrappers.lambdaQuery();
        wrapper.in(UserRole::getRoleId, idList);
        remove(wrapper);
	}
	
	@Override
	public void deleteByUserIds(Collection<Long> idList) {
        LambdaQueryWrapper<UserRole> wrapper =  Wrappers.lambdaQuery();
        wrapper.in(UserRole::getUserId, idList);
        remove(wrapper);
	}
	
	@Override
    public List<Long> findRoleIdListByUserId(Long userId) {
        return ConvertUtils.convert(findByUserId(userId), pu -> pu.getRoleId());
    }
	
	private List<UserRole> findByUserId(Long userId) {
        LambdaQueryWrapper<UserRole> wrapper =  Wrappers.lambdaQuery();
        wrapper.eq(UserRole::getUserId, userId);
        return list(wrapper);
    }
}
