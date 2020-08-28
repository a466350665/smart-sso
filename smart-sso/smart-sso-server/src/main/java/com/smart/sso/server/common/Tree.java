package com.smart.sso.server.common;

import java.util.Collections;
import java.util.List;
import java.util.function.Function;

import org.springframework.util.CollectionUtils;

import com.google.common.collect.Lists;
import com.smart.sso.server.dto.TreeDto;

/**
 * 树接口
 * 
 * @author Joe
 */
public interface Tree {
	
	Integer getParentId();
	
	Integer getId();
	
	public static <T extends Tree, E extends TreeDto> List<E> build(List<T> list,
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

	public static <T extends Tree, E extends TreeDto> void loopSubList(E treeDto, List<T> list,
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