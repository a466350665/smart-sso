package openjoe.smart.sso.server.service.impl;

import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import openjoe.smart.sso.base.entity.Result;
import openjoe.smart.sso.server.entity.App;
import openjoe.smart.sso.server.manager.AppManager;
import openjoe.smart.sso.server.mapper.AppMapper;
import openjoe.smart.sso.server.service.AppService;
import openjoe.smart.sso.server.service.PermissionService;
import openjoe.smart.sso.server.service.RolePermissionService;
import openjoe.smart.stage.core.entity.Page;
import openjoe.smart.stage.mybatisplus.service.impl.BaseServiceImpl;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.CollectionUtils;

import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

@Service("appService")
public class AppServiceImpl extends BaseServiceImpl<AppMapper, App> implements AppService, AppManager {
	
	@Autowired
	private PermissionService permissionService;
	@Autowired
	private RolePermissionService rolePermissionService;

	@Override
    @Transactional
    public void enable(Boolean isEnable, List<Long> idList) {
        selectByIds(idList).forEach(t -> {
            t.setIsEnable(isEnable);
            updateById(t);
        });
    }

	private List<App> selectByIds(Collection<Long> idList){
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
	@Transactional
	public void deleteByIds(Collection<Long> idList) {
		rolePermissionService.deleteByAppIds(idList);
		permissionService.deleteByAppIds(idList);
		super.removeByIds(idList);
	}

	@Override
	public Map<String, App> selectMapByClientIds(Collection<String> clientIdList) {
		LambdaQueryWrapper<App> wrapper =  Wrappers.lambdaQuery();
		wrapper.in(App::getClientId, clientIdList);
		List<App> list = list(wrapper);
		if (CollectionUtils.isEmpty(list)) {
			return Collections.emptyMap();
		}
		return list.stream().collect(Collectors.toMap(App::getClientId, t->t));
	}

	@Override
	public String generateClientId() {
		LambdaQueryWrapper<App> wrapper = Wrappers.lambdaQuery();
		wrapper.orderByDesc(App::getClientId);
		App app = getOne(wrapper, false);
		if (app == null) {
			return "1000";
		} else {
			return String.valueOf(Integer.valueOf(app.getClientId()) + 1);
		}
	}

	@Override
	public App selectByClientId(String clientId) {
		LambdaQueryWrapper<App> wrapper = Wrappers.lambdaQuery();
		wrapper.eq(App::getClientId, clientId);
		App app = getOne(wrapper);
		if (app != null && app.getIsEnable()) {
			return app;
		} else {
			return null;
		}
	}

	@Override
	public Result<Long> validate(String clientId) {
		App app = selectByClientId(clientId);
		if(app == null){
			return Result.error("非法应用");
		}
		return Result.success(app.getId());
	}

	@Override
	public Result<Void> validate(String clientId, String clientSecret) {
		App app = selectByClientId(clientId);
		if(app == null){
			return Result.error("clientId不存在");
		}
		if (!app.getClientSecret().equals(clientSecret)) {
			return Result.error("appSecret有误");
		}
		return Result.success();
	}
}
