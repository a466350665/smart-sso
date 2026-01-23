package openjoe.smart.sso.server.entity;


import com.baomidou.mybatisplus.annotation.TableName;
import openjoe.smart.stage.mybatisplus.entity.BaseEntity;

/**
 * 权限
 * 
 * @author Joe
 */
@TableName("sys_permission")
public class Permission extends BaseEntity implements Tree {

	/** 应用ID */
	private Long appId;
	/** 父ID */
	private Long parentId;
	/** 图标 */
	private String icon;
	/** 名称 */
	private String name;
	/** 权限URL */
	private String url;
	/** 排序 */
	private Integer sort;
	/** 是否菜单 */
	private Boolean isMenu;
	/** 是否启用 */
	private Boolean isEnable;
	
	public Long getAppId() {
		return this.appId;
	}

	public void setAppId(Long appId) {
		this.appId = appId;
	}

	@Override
	public Long getParentId() {
		return this.parentId;
	}

	public void setParentId(Long parentId) {
		this.parentId = parentId;
	}
	
	public String getIcon() {
		return icon;
	}

	public void setIcon(String icon) {
		this.icon = icon;
	}

	public String getName() {
		return this.name;
	}

	public void setName(String name) {
		this.name = name;
	}
	
	public String getUrl() {
		return url;
	}

	public void setUrl(String url) {
		this.url = url;
	}

	public Integer getSort() {
		return this.sort;
	}

	public void setSort(Integer sort) {
		this.sort = sort;
	}

	public Boolean getIsMenu() {
		return this.isMenu;
	}

	public void setIsMenu(Boolean isMenu) {
		this.isMenu = isMenu;
	}

	public Boolean getIsEnable() {
		return this.isEnable;
	}

	public void setIsEnable(Boolean isEnable) {
		this.isEnable = isEnable;
	}
	
	public String getUrlStr() {
		return url;
	}
	
	public String getPermissionIcon() {
		return icon;
	}
}
