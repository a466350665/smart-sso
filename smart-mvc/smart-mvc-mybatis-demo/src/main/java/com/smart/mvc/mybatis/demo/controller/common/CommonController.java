package com.smart.mvc.mybatis.demo.controller.common;

import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;

import com.smart.mvc.controller.BaseController;

/**
 * 公用控制
 * 
 * @author Joe
 */
@Controller
@RequestMapping("/common/common")
public class CommonController extends BaseController {

	/**
	 * 没访问权限跳转页
	 * @return
	 */
	@RequestMapping(value = "noPermission", method = RequestMethod.GET)
	public String noPermission() {
		return "/noPermission";
	}
	
	/**
	 * 系统公用错误页
	 * @return
	 */
	@RequestMapping(value = "error", method = RequestMethod.GET)
	public String error() {
		return "/error";
	}
}