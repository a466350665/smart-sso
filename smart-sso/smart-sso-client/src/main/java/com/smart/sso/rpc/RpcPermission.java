package com.smart.sso.rpc;

import java.io.Serializable;

/**
 * RPC回传权限对象(含菜单)
 * 
 * @author Joe
 */
public class RpcPermission implements Serializable {

	private static final long serialVersionUID = 6413358335961655343L;

	/** ID */
	private Integer id;
	/** 父ID */
	private Integer parentId;
	/** 图标 */
	private String icon;
	/** 名称 */
	private String name;
	/** 权限URL */
	private String url;
	/** 是否菜单 */
	private Boolean isMenu;

	public Integer getId() {
		return id;
	}

	public void setId(Integer id) {
		this.id = id;
	}

	public Integer getParentId() {
		return parentId;
	}

	public void setParentId(Integer parentId) {
		this.parentId = parentId;
	}

	public String getIcon() {
		return icon;
	}

	public void setIcon(String icon) {
		this.icon = icon;
	}

	public String getName() {
		return name;
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

	public Boolean getIsMenu() {
		return isMenu;
	}

	public void setIsMenu(Boolean isMenu) {
		this.isMenu = isMenu;
	}
}
