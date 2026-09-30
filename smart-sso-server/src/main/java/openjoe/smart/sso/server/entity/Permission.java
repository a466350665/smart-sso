package openjoe.smart.sso.server.entity;


import com.baomidou.mybatisplus.annotation.FieldStrategy;
import com.baomidou.mybatisplus.annotation.TableField;
import com.baomidou.mybatisplus.annotation.TableName;
import openjoe.smart.stage.mybatisplus.entity.BaseEntity;

/**
 * 权限
 * 
 * @author Joe
 */
@TableName
public class Permission extends BaseEntity implements Tree {

	/** 应用ID */
	private Long appId;
	/** 父ID（允许为null，表示顶级权限；更新时必须强制写入，否则会被MyBatis-Plus的NOT_NULL策略忽略） */
	@TableField(updateStrategy = FieldStrategy.ALWAYS)
	private Long parentId;
	/** 图标 */
	private String icon;
	/** 名称 */
	private String name;
	/** 权限编码 */
	private String code;
	/** 排序 */
	private Integer sort;
	/** 权限类型：menu=菜单，function=功能（见 PermissionTypeEnum） */
	private String type;
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
	
	public String getCode() {
		return code;
	}

	public void setCode(String code) {
		this.code = code;
	}

	public Integer getSort() {
		return this.sort;
	}

	public void setSort(Integer sort) {
		this.sort = sort;
	}

	public String getType() {
		return this.type;
	}

	public void setType(String type) {
		this.type = type;
	}

	public Boolean getIsEnable() {
		return this.isEnable;
	}

	public void setIsEnable(Boolean isEnable) {
		this.isEnable = isEnable;
	}
	
	public String getPermissionIcon() {
		return icon;
	}
}
