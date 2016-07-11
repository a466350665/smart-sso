package com.${company!''}.${project!''}.${module!''}.action;

<#if containEnable>
import java.util.ArrayList;
</#if>
import java.util.List;

import javax.annotation.Resource;

import org.springframework.context.annotation.Scope;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;
import org.springframework.beans.BeanUtils;

import com.${company!''}.${project!''}.sys.action.SysAdminAction;
import com.${company!''}.${project!''}.${module!''}.model.${model};
import com.${company!''}.${project!''}.${module!''}.service.${model}Service;
import com.${company!''}.ssh.model.Pagination;
import com.${company!''}.util.StringUtils;

@Component
@Scope("prototype")
public class ${model}Action extends SysAdminAction {

	private static final long serialVersionUID = 7084551381591001347L;

	private ${model} ${_model};

	@Resource
	private ${model}Service ${_model}Service;

	@Override
	public String execute() throws Exception {
		return dispatcher("/${module!''}/${_model}.jsp");
	}

	public void list() {
		Pagination<${model}> p = new Pagination<${model}>(pageNo, rowSize);
		${_model}Service.findPagination(p);
		ajaxDateJson(p);
	}

	public String edit() {
		if (${_model} != null && ${_model}.getId() != null) {
			${_model} = ${_model}Service.get(${_model}.getId());
		}
		else {
			${_model} = new ${model}();
		}
		return dispatcher("/${module!''}/${_model}Edit.jsp");
	}

	public void save() {
		if (${_model} != null && StringUtils.isNotBlank(${_model}.getId())) {
			${model} db = ${_model}Service.get(${_model}.getId());
			BeanUtils.copyProperties(${_model}, db, new String[] {"id"});
			${_model}Service.save(db);
		}
		else {
			${_model}Service.save(${_model});
		}
		ajaxJsonSuccess();
	}
	
	<#if containEnable>
	public void enable() {
		List<${model}> list = ${_model}Service.get(getAjaxIds());
		if (!CollectionUtils.isEmpty(list)) {
			List<${model}> aList = new ArrayList<${model}>();
			for (${model} a : list) {
				if (!a.getIsEnable().equals(isEnable)) {
					a.setIsEnable(isEnable);
					aList.add(a);
				}
			}
			${_model}Service.save(aList);
			ajaxJsonSuccess();
		}
		else {
			ajaxJsonError();
		}
	}
	</#if>

	public void delete() {
		List<${model}> list = ${_model}Service.get(getAjaxIds());
		if (!CollectionUtils.isEmpty(list)) {
			${_model}Service.delete(list);
			ajaxJsonSuccess();
		}
		else {
			ajaxJsonError();
		}
	}

	public ${model} get${model}() {
		return ${_model};
	}

	public void set${model}(${model} ${_model}) {
		this.${_model} = ${_model};
	}
}
