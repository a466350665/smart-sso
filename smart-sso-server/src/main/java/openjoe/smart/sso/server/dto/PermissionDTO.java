package openjoe.smart.sso.server.dto;

/**
 * 权限树节点
 * <p>
 * 注意：这里只额外暴露页面渲染必须的 isMenu（权限管理页用它决定是否显示“添加子权限”按钮）。
 * 不要暴露 icon / url：
 *  - zTree 会把 node.icon 当图片路径、把 node.url 当超链接，会引发无谓的 404 请求与误跳转；
 *  - 编辑表单所需的 url/sort/icon/isEnable 走 /admin/permission/get 获取完整实体。
 */
public class PermissionDTO extends TreeDTO {

    private Boolean checked;
    /** 是否菜单 */
    private Boolean isMenu;

    public Boolean getChecked() {
        return checked;
    }

    public void setChecked(Boolean checked) {
        this.checked = checked;
    }

    public Boolean getIsMenu() {
        return isMenu;
    }

    public void setIsMenu(Boolean isMenu) {
        this.isMenu = isMenu;
    }
}
