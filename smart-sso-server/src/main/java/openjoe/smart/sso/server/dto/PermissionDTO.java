package openjoe.smart.sso.server.dto;

/**
 * 权限树节点
 * <p>
 * 注意：这里只额外暴露页面渲染必须的 type（权限管理页用它区分菜单/功能的展示样式）。
 * 不要暴露 icon / code：
 *  - zTree 会把 node.icon 当图片路径，会引发无谓的 404 请求；
 *  - 编辑表单所需的 code/type/sort/icon/isEnable 走 /admin/permission/get 获取完整实体。
 */
public class PermissionDTO extends TreeDTO {

    private Boolean checked;
    /** 权限类型：menu=菜单，function=功能 */
    private String type;

    public Boolean getChecked() {
        return checked;
    }

    public void setChecked(Boolean checked) {
        this.checked = checked;
    }

    public String getType() {
        return type;
    }

    public void setType(String type) {
        this.type = type;
    }
}
