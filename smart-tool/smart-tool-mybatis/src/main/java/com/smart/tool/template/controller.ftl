package com.${company!''}.${project!''}.<#if module??>${module}.</#if>controller<#if admin??>.${admin}</#if>;

import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiParam;

import javax.annotation.Resource;
<#if containDate>
import java.util.Date;

import org.springframework.format.annotation.DateTimeFormat;
<#else>

</#if>
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.ResponseBody;

import com.${company!''}.${project!''}.<#if module??>${module}.</#if>model.${model};
import com.${company!''}.${project!''}.<#if module??>${module}.</#if>service.${model}Service;
import com.smart.mvc.controller.BaseController;
import com.smart.mvc.model.Result;
import com.smart.mvc.model.Pagination;
import com.smart.mvc.validator.Validator;
import com.smart.mvc.validator.annotation.ValidateParam;

/**
 * @author Joe
 */
@Api(tags = "${tableComment}")
@Controller
@RequestMapping("<#if admin??>/${admin}</#if>/${_model}")
public class ${model}Controller extends BaseController {

	@Resource
	private ${model}Service ${_model}Service;

	@ApiOperation("初始页")
	@RequestMapping(method = RequestMethod.GET)
	public String execute(Model model) {
		return "<#if admin??>/${admin}</#if>/${_model}";
	}
	
	@ApiOperation("列表")
	@RequestMapping(value = "/list", method = RequestMethod.GET)
	public @ResponseBody Result list(
			@ApiParam(value = "开始页码", required = true) @ValidateParam({ Validator.NOT_BLANK }) Integer pageNo,
			@ApiParam(value = "显示条数", required = true) @ValidateParam({ Validator.NOT_BLANK }) Integer pageSize) {
		return Result.createSuccessResult().setData(${_model}Service.findByAllPagination(new Pagination<${model}>(pageNo, pageSize)));
	}

	@ApiOperation("新增/修改页")
	@RequestMapping(value = "/edit", method = RequestMethod.GET)
	public String edit(@ApiParam(value = "id") Integer id, Model model) {
		${model} ${_model};
		if (id == null) {
			${_model} = new ${model}();
		}
		else {
			${_model} = ${_model}Service.get(id);
		}
		model.addAttribute("${_model}", ${_model});
		return "<#if admin??>/${admin}</#if>/${_model}Edit";
	}

	@ApiOperation("新增/修改提交")
	@RequestMapping(value = "/save", method = RequestMethod.POST)
	public @ResponseBody Result save(
			@ApiParam(value = "id") Integer id,
		<#list fieldList as field>
			@ApiParam(<#if field.description??>value = "${field.description}"</#if><#if field.nullableStr == "false">, required = true</#if>)<#if field.nullableStr == "false"> @ValidateParam({ Validator.NOT_BLANK })</#if><#if field.fieldType == "Date"> @DateTimeFormat(pattern = "yyyy-MM-dd HH:mm:ss") Date<#else> ${field.fieldType}</#if> ${field.fieldName}<#if field_has_next>,</#if>
		</#list>
			) {
		${model} ${_model};
		if (id == null) {
			${_model} = new ${model}();
		}
		else {
			${_model} = ${_model}Service.get(id);
		}
		<#list fieldList as field>
		${_model}.set${field.upperFieldName}(${field.fieldName});
		</#list>
		${_model}Service.save(${_model});
		return Result.createSuccessResult();
	}

	@ApiOperation("删除")
	@RequestMapping(value = "/delete", method = RequestMethod.POST)
	public @ResponseBody Result delete(
			@ApiParam(value = "ids", required = true) @ValidateParam({ Validator.NOT_BLANK }) String ids) {
		${_model}Service.deleteById(getAjaxIds(ids));
		return Result.createSuccessResult();
	}
}