package openjoe.smart.sso.server.enums;

/**
 * 权限类型（仅两种，数据库存英文值）
 *
 * @author Joe
 */
public enum PermissionTypeEnum {

    /** 菜单 */
    MENU("menu", "菜单"),
    /** 功能（按钮/接口等非菜单权限） */
    FUNCTION("function", "功能");

    private final String value;
    private final String desc;

    PermissionTypeEnum(String value, String desc) {
        this.value = value;
        this.desc = desc;
    }

    public String getValue() {
        return value;
    }

    public String getDesc() {
        return desc;
    }

    /**
     * 是否为合法权限类型（存库用的英文值）
     */
    public static boolean isValid(String value) {
        for (PermissionTypeEnum type : values()) {
            if (type.value.equals(value)) {
                return true;
            }
        }
        return false;
    }

    /**
     * 是否菜单类型
     */
    public static boolean isMenu(String value) {
        return MENU.value.equals(value);
    }

    /**
     * 是否为功能权限
     */
    public static boolean isFunction(String value) {
        return FUNCTION.value.equals(value);
    }
}
