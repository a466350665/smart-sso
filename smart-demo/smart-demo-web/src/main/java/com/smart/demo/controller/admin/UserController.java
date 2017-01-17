package com.smart.demo.controller.admin;

import javax.annotation.Resource;

import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.ResponseBody;

import com.smart.demo.model.User;
import com.smart.demo.service.UserService;
import com.smart.mvc.controller.BaseController;
import com.smart.mvc.model.Pagination;
import com.smart.mvc.model.Result;
import com.smart.mvc.model.ResultCode;
import com.smart.mvc.validator.Validator;
import com.smart.mvc.validator.annotation.ValidateParam;
import com.smart.util.StringUtils;

/**
 * 管理员管理
 * 
 * @author Joe
 */
@Controller
@RequestMapping("/admin/user")
public class UserController extends BaseController {

	@Resource
	private UserService userService;

	/**	
	 * 管理员初始页
	 * @param model
	 * @return
	 */
	@RequestMapping(method = RequestMethod.GET)
	public String execute(Model model) {
		return "/admin/user";
	}
	
	/**
	 * ajax读取表格数据
	 * @param account 登录名
	 * @param pageNo 开始页码
	 * @param pageSize 显示条数
	 * @return
	 */
	@RequestMapping(value = "/list", method = RequestMethod.GET)
	public @ResponseBody Result list(
			@ValidateParam(name = "登录名 ") String account,
			@ValidateParam(name = "开始页码", validators = { Validator.NOT_BLANK }) Integer pageNo,
			@ValidateParam(name = "显示条数", validators = { Validator.NOT_BLANK }) Integer pageSize) {
		return Result.createSuccessResult().setData(userService.findPaginationByAccount(account, new Pagination<User>(pageNo, pageSize)));
	}
	
	@RequestMapping(value = "/validateAccount", method = RequestMethod.POST)
	public @ResponseBody Result validateAccount(
			@ValidateParam(name = "id") Integer id,
			@ValidateParam(name = "登录名 ", validators = { Validator.NOT_BLANK }) String account) {
		Result result = Result.createSuccessResult();
		if (StringUtils.isNotBlank(account)) {
			User user = userService.findByAccount(account);
			if (null != user && !user.getId().equals(id)) {
				result.setCode(ResultCode.ERROR).setMessage("登录名已存在");
			}
		}
		return result;
	}

	/**
	 * 编辑按钮(含添加和修改两种操作)
	 * @param id 添加时id为空
	 * @param model
	 * @return
	 */
	@RequestMapping(value = "/edit", method = RequestMethod.GET)
	public String edit(@ValidateParam(name = "id") Integer id, Model model) {
		User user;
		if (id == null) {
			user = new User();
		}
		else {
			user = userService.get(id);
		}
		model.addAttribute("user", user);
		return "/admin/userEdit";
	}

	/**
	 * 保存(含添加和更新两种操作)
	 * @param id 添加时id为空
	 * @param account 登录名
	 * @return
	 */
	@RequestMapping(value = "/save", method = RequestMethod.POST)
	public @ResponseBody Result save(
			@ValidateParam(name = "ID") Integer id,
			@ValidateParam(name = "登录名", validators = { Validator.NOT_BLANK }) String account) {
		User user;
		if (id == null) {
			user = new User();
		}
		else {
			user = userService.get(id);
		}
		user.setAccount(account);
		userService.saveOrUpdate(user);
		return Result.createSuccessResult();
	}

	/**
	 * 删除(根据id删除，含多条删除情况)
	 * @param ids
	 * @return
	 */
	@RequestMapping(value = "/delete", method = RequestMethod.POST)
	public @ResponseBody Result delete(@ValidateParam(name = "ids", validators = { Validator.NOT_BLANK }) String ids) {
		userService.deleteById(getAjaxIds(ids));
		return Result.createSuccessResult();
	}
}