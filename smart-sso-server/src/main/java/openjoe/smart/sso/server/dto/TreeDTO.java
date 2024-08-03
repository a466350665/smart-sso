package openjoe.smart.sso.server.dto;

import openjoe.smart.sso.server.stage.core.Tree;
import org.springframework.util.CollectionUtils;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.function.Function;

/**
 * 树DTO
 *
 * @author Joe
 */
public class TreeDTO implements Tree {
    // ID
    private Long id;
    // 父ID
    private Long parentId;
    // 名称
    private String name;
    // 孩子节点
    private List<TreeDTO> children = new ArrayList<>();

    public TreeDTO() {
        super();
    }

    public TreeDTO(Long id, Long parentId, String name) {
        super();
        this.id = id;
        this.parentId = parentId;
        this.name = name;
    }

    @Override
    public Long getId() {
        return id;
    }

    public void setId(Long id) {
        this.id = id;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    @Override
    public Long getParentId() {
        return parentId;
    }

    public void setParentId(Long parentId) {
        this.parentId = parentId;
    }

    public List<TreeDTO> getChildren() {
        return children;
    }

    public void setChildren(List<TreeDTO> children) {
        this.children = children;
    }

    public static <T extends TreeDTO, E extends TreeDTO> List<E> convert(List<T> list,
                                                                         Function<? super T, ? extends E> function) {
        if (CollectionUtils.isEmpty(list)) {
            return Collections.emptyList();
        }
        List<E> treeList = new ArrayList<>();
        for (T p : list) {
            if (p.getParentId() == null || Long.valueOf(0).equals(p.getParentId())) {
                E treeDto = function.apply(p);
                loopSubList(treeDto, list, function);
                treeList.add(treeDto);
            }
        }
        return treeList;
    }

    public static <T extends TreeDTO, E extends TreeDTO> void loopSubList(E treeDto, List<T> list,
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