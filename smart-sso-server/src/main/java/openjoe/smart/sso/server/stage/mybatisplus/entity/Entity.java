package openjoe.smart.sso.server.stage.mybatisplus.entity;

import com.baomidou.mybatisplus.annotation.IdType;
import com.baomidou.mybatisplus.annotation.TableId;
import io.swagger.annotations.ApiModelProperty;

/**
 * MybatisPlus基础持久化基类
 *
 * @author Joe
 */
public class Entity {

    @ApiModelProperty("主键")
    @TableId(type = IdType.AUTO)
    private Long id;

    public Long getId() {
        return id;
    }

    public void setId(Long id) {
        this.id = id;
    }
}