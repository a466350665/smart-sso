package com.smart.sso.server.service.impl;

import com.smart.sso.base.entity.Result;
import com.smart.sso.server.model.App;
import com.smart.sso.server.service.AppService;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.List;

@Service("appService")
public class AppServiceImpl implements AppService {

	private static List<App> appList;

	static {
		appList = new ArrayList<>();
		appList.add(new App("客户端1", "demo1", "123456"));
		appList.add(new App("客户端2", "demo2", "123456"));
	}

	@Override
	public boolean exists(String appId) {
		return appList.stream().anyMatch(app -> app.getAppId().equals(appId));
	}

	@Override
	public Result<Void> validate(String appId, String appSecret) {
		for (App app : appList) {
			if (app.getAppId().equals(appId)) {
				// 此处为简化校验操作（生产场景应该使用应用登记时生成的非对称密钥对方式校验）
				if (app.getAppSecret().equals(appSecret)) {
					return Result.success();
				}
				else {
					return Result.createError("appSecret有误");
				}
			}
		}
		return Result.createError("appId不存在");
	}
}
