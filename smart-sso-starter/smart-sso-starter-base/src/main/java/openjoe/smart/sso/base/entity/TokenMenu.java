package openjoe.smart.sso.base.entity;

/**
 * 菜单对象
 * 
 * @author Joe
 */
public class TokenMenu {

	/** ID */
	private Long id;
	/** 父ID */
	private Long parentId;
	/** 图标 */
	private String icon;
	/** 名称 */
	private String name;
	/** 权限URL */
	private String url;

	public Long getId() {
		return id;
	}

	public void setId(Long id) {
		this.id = id;
	}

	public Long getParentId() {
		return parentId;
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
}