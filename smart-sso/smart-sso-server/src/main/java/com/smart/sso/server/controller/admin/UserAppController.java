package com.smart.sso.server.controller.admin;

import java.util.ArrayList;
import java.util.List;

import javax.annotation.Resource;
import javax.servlet.http.HttpServletRequest;

import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.ResponseBody;

import com.smart.mvc.controller.BaseController;
import com.smart.mvc.model.Result;
import com.smart.mvc.validator.Validator;
import com.smart.mvc.validator.annotation.ValidateParam;
import com.smart.sso.server.model.App;
import com.smart.sso.server.model.UserApp;
import com.smart.sso.server.service.AppService;
import com.smart.sso.server.service.UserAppService;

/**
 * 管理员管理
 * 
 * @author Joe
 */
@Controller
@RequestMapping("/admin/userApp")
public class UserAppController extends BaseController {

	@Resource
	private AppService appService;
	@Resource
	private UserAppService userAppService;
	
	@RequestMapping(value = "/allocate", method = RequestMethod.GET)
	public String edit(@ValidateParam(name = "管理员Id", validators = { Validator.NOT_BLANK }) Integer userId, Model model) {
		model.addAttribute("userId", userId);
		model.addAttribute("appList", getAppList(userId));
		return "/admin/userApp";
	}

	@RequestMapping(value = "/allocateSave", method = RequestMethod.POST)
	public @ResponseBody Result allocateSave(
			@ValidateParam(name = "管理员ID", validators = { Validator.NOT_BLANK }) Integer userId,
			@ValidateParam(name = "应用IDS") String appIds,
			HttpServletRequest request) {
		List<Integer> idList = getAjaxIds(appIds);
		List<UserApp> list = new ArrayList<UserApp>();
		UserApp bean = null;
		for (Integer appId : idList) {
			bean = new UserApp();
			bean.setAppId(appId);
			bean.setUserId(userId);
			list.add(bean);
		}
		return Result.create("授权成功").setData(userAppService.allocate(userId, idList, list));
	}
	
	private List<App> getAppList(Integer userId) {
		List<App> list = appService.findByAll(null);
		for (App app : list) {
			UserApp userApp = userAppService.findByUserAppId(userId, app.getId());
			if (null != userApp) {
				app.setIsChecked(true);
			}
			else {
				app.setIsChecked(false);
			}
		}
		return list;
	}
}