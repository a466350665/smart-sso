package com.smart.sso.server.dto;

import java.io.Serializable;
import java.util.Collections;
import java.util.List;
import java.util.function.Function;

import org.springframework.util.CollectionUtils;

import com.google.common.collect.Lists;
import com.smart.sso.server.common.Tree;

/**
 * 菜单DTO
 * 
 * @author Joe
 */
public class TreeDto implements Tree, Serializable {

    private static final long serialVersionUID = -1044939782544438879L;
    // ID
    private Integer id;
    // 父ID
    private Integer parentId;
    // 名称
    private String name;
    // 孩子节点
    private List<TreeDto> children = Lists.newArrayList();

    public TreeDto() {
        super();
    }

    public TreeDto(Integer id, Integer parentId, String name) {
        super();
        this.id = id;
        this.parentId = parentId;
        this.name = name;
    }

    public Integer getId() {
        return id;
    }

    public void setId(Integer id) {
        this.id = id;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public Integer getParentId() {
        return parentId;
    }

    public void setParentId(Integer parentId) {
        this.parentId = parentId;
    }

    public List<TreeDto> getChildren() {
        return children;
    }

    public void setChildren(List<TreeDto> children) {
        this.children = children;
    }
    
	public static <T extends TreeDto, E extends TreeDto> List<E> convert(List<T> list,
			Function<? super T, ? extends E> function) {
		if (CollectionUtils.isEmpty(list)) {
			return Collections.emptyList();
		}
		List<E> treeList = Lists.newArrayList();
		for (T p : list) {
			if (p.getParentId() == null || Integer.valueOf(0).equals(p.getParentId())) {
				E treeDto = function.apply(p);
				loopSubList(treeDto, list, function);
				treeList.add(treeDto);
			}
		}
		return treeList;
	}

	public static <T extends TreeDto, E extends TreeDto> void loopSubList(E treeDto, List<T> list,
			Function<? super T, ? extends E> createFunction) {
		for (T p : list) {
			if (treeDto.getId().equals(p.getParentId())) {
				E subDto = createFunction.apply(p);
				treeDto.getChildren().add(subDto);
				loopSubList(subDto, list, createFunction);
			}
		}
	}
}