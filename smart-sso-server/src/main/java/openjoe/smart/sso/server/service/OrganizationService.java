package openjoe.smart.sso.server.service;

import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import openjoe.smart.sso.server.entity.Organization;
import openjoe.smart.sso.server.mapper.OrganizationMapper;
import openjoe.smart.stage.mybatisplus.service.BaseService;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.CollectionUtils;
import org.springframework.util.StringUtils;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;

@Service
public class OrganizationService extends BaseService<OrganizationMapper, Organization> {

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

    @Transactional
    public void deleteByIds(Collection<Long> idList) {
        if (CollectionUtils.isEmpty(idList)) {
            return;
        }
        removeByIds(idList);
    }

    private List<Organization> selectByIds(List<Long> idList){
        LambdaQueryWrapper<Organization> wrapper =  Wrappers.lambdaQuery();
        wrapper.in(Organization::getId, idList);
        return list(wrapper);
    }

	public List<Organization> selectList(Boolean isEnable, Boolean isParent, Long currentId, String prefix) {
		List<Organization> list = selectList(isEnable, isParent, currentId);
		if (StringUtils.hasLength(prefix)) {
			List<Organization> dataList = new ArrayList<>();
			for (Organization organization : list) {
				if (organization.getParentId() == null) {
					dataList.add(organization);
					buildTree(organization.getId(), list, dataList, prefix, prefix);
				}
			}
			list = dataList;
		}
		return list;
	}
	
	private void buildTree(Long organizationId, List<Organization> list, List<Organization> dataList, String currentPrefix, String prefix){  
        List<Organization> subList = getSubList(organizationId, list, currentPrefix);
        if (!subList.isEmpty()) {  
            for (Organization organization : subList) {
            	dataList.add(organization);
                buildTree(organization.getId(), list, dataList, prefix + currentPrefix, prefix);  
            }  
        }   
    }  
      
    private List<Organization> getSubList(Long organizationId, List<Organization> list, String currentPrefix){  
        List<Organization> children = new ArrayList<>();
        for (Organization child : list) {
            if (organizationId.equals(child.getParentId())) {
            	child.setName(currentPrefix + child.getName());
                children.add(child);  
            }  
        }  
        return children;  
    }

	public List<Long> selectIdListByParentId(Long parentId) {
		if (parentId == null){
            return Collections.emptyList();
        }
		List<Long> idList = new ArrayList<>();
		idList.add(parentId);
		List<Organization> list = selectList(true, null, null);
		if (!CollectionUtils.isEmpty(list)) {
			buildTree(parentId, list, idList);
		}
		return idList;
	}
	
	private void buildTree(Long organizationId, List<Organization> list, List<Long> idList){
        List<Organization> subList = getSubList(organizationId, list, "");
        if (!subList.isEmpty()) {  
            for (Organization organization : subList) {
            	idList.add(organization.getId());
                buildTree(organization.getId(), list, idList);  
            }  
        }   
    } 
	
    private List<Organization> selectList(Boolean isEnable, Boolean isParent, Long currentId) {
        LambdaQueryWrapper<Organization> wrapper =  Wrappers.lambdaQuery();
        wrapper.eq(isEnable != null, Organization::getIsEnable, isEnable);
        wrapper.isNull(isParent != null && isParent, Organization::getParentId);
        wrapper.ne(currentId != null, Organization::getId, currentId);
        return list(wrapper);
    }
}
