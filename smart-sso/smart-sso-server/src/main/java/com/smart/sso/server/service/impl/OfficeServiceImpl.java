package com.smart.sso.server.service.impl;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;

import com.smart.mvc.service.mybatis.impl.ServiceImpl;
import com.smart.mvc.util.StringUtils;
import com.smart.sso.server.dao.OfficeDao;
import com.smart.sso.server.model.Office;
import com.smart.sso.server.service.OfficeService;

@Component("officeService")
public class OfficeServiceImpl extends ServiceImpl<OfficeDao, Office, Integer> implements OfficeService {

	@Autowired
	public void setDao(OfficeDao dao) {
		this.dao = dao;
	}

	public void enable(Boolean isEnable, List<Integer> idList) {
		verifyRows(dao.enable(isEnable, idList), idList.size(), "管理员数据库更新失败");
	}

	@Override
	public List<Office> findByParams(Boolean isEnable, Boolean isParent, Integer currentId, String prefix) {
		List<Office> list = dao.findByParams(isEnable, isParent, currentId);
		if (StringUtils.isNotBlank(prefix)) {
			List<Office> dataList = new ArrayList<Office>();
			for (Office office : list) {
				if (office.getParentId() == null) {
					dataList.add(office);
					buildTree(office.getId(), list, dataList, prefix, prefix);
				}
			}
			list = dataList;
		}
		return list;
	}
	
	private void buildTree(Integer officeId, List<Office> list, List<Office> dataList, String currentPrefix, String prefix){  
        List<Office> subList = getSubList(officeId, list, currentPrefix);
        if (!subList.isEmpty()) {  
            for (Office office : subList) {
            	dataList.add(office);
                buildTree(office.getId(), list, dataList, prefix + currentPrefix, prefix);  
            }  
        }   
    }  
      
    private List<Office> getSubList(Integer officeId, List<Office> list, String currentPrefix){  
        List<Office> children = new ArrayList<Office>();
        for (Office child : list) {
            if (officeId.equals(child.getParentId())) {
            	child.setName(currentPrefix + child.getName());
                children.add(child);  
            }  
        }  
        return children;  
    }

	@Override
	public List<Integer> findIdListByParentId(Integer parentId) {
		if (parentId == null)
			return Collections.emptyList();
		List<Integer> idList = new ArrayList<>();
		idList.add(parentId);
		List<Office> list = dao.findByParams(true, null, null);
		if (!CollectionUtils.isEmpty(list)) {
			buildTree(parentId, list, idList);
		}
		return idList;
	}
	
	private void buildTree(Integer officeId, List<Office> list, List<Integer> idList){
        List<Office> subList = getSubList(officeId, list, "");
        if (!subList.isEmpty()) {  
            for (Office office : subList) {
            	idList.add(office.getId());
                buildTree(office.getId(), list, idList);  
            }  
        }   
    }  
}
