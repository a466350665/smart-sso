package openjoe.smart.sso.server.common;

import com.google.common.collect.Lists;
import openjoe.smart.sso.server.dto.TreeDTO;
import org.springframework.util.CollectionUtils;

import java.util.Collections;
import java.util.List;
import java.util.function.Function;

/**
 * 树接口
 * 
 * @author Joe
 */
public interface Tree {
	
	Long getParentId();
	
	Long getId();
	
	static <T extends Tree, E extends TreeDTO> List<E> build(List<T> list,
															 Function<? super T, ? extends E> function) {
		if (CollectionUtils.isEmpty(list)) {
			return Collections.emptyList();
		}
		List<E> treeList = Lists.newArrayList();
		for (T p : list) {
			if (p.getParentId() == null || Long.valueOf(0).equals(p.getParentId())) {
				E treeDto = function.apply(p);
				loopSubList(treeDto, list, function);
				treeList.add(treeDto);
			}
		}
		return treeList;
	}

	static <T extends Tree, E extends TreeDTO> void loopSubList(E treeDto, List<T> list,
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