package openjoe.smart.sso.server.service;

import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import openjoe.smart.sso.base.entity.Result;
import openjoe.smart.sso.server.entity.App;
import openjoe.smart.sso.server.manager.AppManager;
import openjoe.smart.sso.server.mapper.AppMapper;
import openjoe.smart.stage.core.entity.Page;
import openjoe.smart.stage.mybatisplus.service.BaseService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.CollectionUtils;
import org.springframework.util.StringUtils;

import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

@Service
public class AppService extends BaseService<AppMapper, App> implements AppManager {
	
	@Autowired
	private PermissionService permissionService;
	@Autowired
	private RolePermissionService rolePermissionService;

    @Transactional
    public void enable(Boolean isEnable, List<Long> idList) {
        // 空集合会生成非法的 IN () 语句，直接返回
        if (CollectionUtils.isEmpty(idList)) {
            return;
        }
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

	public List<App> selectAll(Boolean isEnable) {
		LambdaQueryWrapper<App> wrapper =  Wrappers.lambdaQuery();
		wrapper.eq(App::getIsEnable, isEnable);
		return list(wrapper);
	}

	public Page<App> selectPage(String name, Long current, Long size) {
		LambdaQueryWrapper<App> wrapper =  Wrappers.lambdaQuery();
		wrapper.like(StringUtils.hasLength(name), App::getName, name);
		return findPage(current, size, wrapper);
	}

	public App selectByCode(String code) {
		LambdaQueryWrapper<App> wrapper =  Wrappers.lambdaQuery();
		wrapper.eq(App::getCode, code);
		return getOne(wrapper);
	}

	@Transactional
	public void deleteByIds(Collection<Long> idList) {
		if (CollectionUtils.isEmpty(idList)) {
			return;
		}
		rolePermissionService.deleteByAppIds(idList);
		permissionService.deleteByAppIds(idList);
		super.removeByIds(idList);
	}

	public Map<String, App> selectMapByClientIds(Collection<String> clientIdList) {
		LambdaQueryWrapper<App> wrapper =  Wrappers.lambdaQuery();
		wrapper.in(App::getClientId, clientIdList);
		List<App> list = list(wrapper);
		if (CollectionUtils.isEmpty(list)) {
			return Collections.emptyMap();
		}
		return list.stream().collect(Collectors.toMap(App::getClientId, t->t));
	}

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
