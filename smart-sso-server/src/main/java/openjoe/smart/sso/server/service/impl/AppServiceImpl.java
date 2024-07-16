package openjoe.smart.sso.server.service.impl;

import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import openjoe.smart.sso.base.entity.Result;
import openjoe.smart.sso.server.entity.App;
import openjoe.smart.sso.server.manager.AppManager;
import openjoe.smart.sso.server.mapper.AppDao;
import openjoe.smart.sso.server.service.AppService;
import openjoe.smart.sso.server.service.PermissionService;
import openjoe.smart.sso.server.service.RolePermissionService;
import openjoe.smart.stage.core.entity.Page;
import openjoe.smart.stage.mybatisplus.service.impl.BaseServiceImpl;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.Collection;
import java.util.List;

@Service("appService")
public class AppServiceImpl extends BaseServiceImpl<AppDao, App> implements AppService, AppManager {
	
	@Autowired
	private PermissionService permissionService;
	@Autowired
	private RolePermissionService rolePermissionService;

	@Override
    @Transactional(readOnly = false)
    public void enable(Boolean isEnable, List<Long> idList) {
        selectByIds(idList).forEach(t -> {
            t.setIsEnable(isEnable);
            updateById(t);
        });
    }

	private List<App> selectByIds(List<Long> idList){
		LambdaQueryWrapper<App> wrapper =  Wrappers.lambdaQuery();
		wrapper.in(App::getId, idList);
		return list(wrapper);
	}
	
	@Override
	public List<App> selectAll(Boolean isEnable) {
		LambdaQueryWrapper<App> wrapper =  Wrappers.lambdaQuery();
		wrapper.eq(App::getIsEnable, isEnable);
		return list(wrapper);
	}

	@Override
	public Page<App> selectPage(String name, Long current, Long size) {
		LambdaQueryWrapper<App> wrapper =  Wrappers.lambdaQuery();
		wrapper.like(App::getName, name);
		return findPage(current, size, wrapper);
	}

	@Override
	public App selectByCode(String code) {
		LambdaQueryWrapper<App> wrapper =  Wrappers.lambdaQuery();
		wrapper.eq(App::getCode, code);
		return getOne(wrapper);
	}
	
	@Override
	@Transactional(readOnly = false)
	public void deleteByIds(Collection<Long> idList) {
		rolePermissionService.deleteByAppIds(idList);
		permissionService.deleteByAppIds(idList);
		super.removeByIds(idList);
	}

	@Override
	public boolean exists(String clientId) {
		return selectByCode(clientId) != null;
	}

	@Override
	public Result<Void> validate(String clientId, String clientSecret) {
		App app = selectByCode(clientId);
		if(app == null){
			return Result.error("appKey不存在");
		}
		// TODO 验证appSecret
//		if (!app.getClientSecret().equals(clientSecret)) {
//			return Result.error("appSecret有误");
//		}
		return Result.success();
	}
}
