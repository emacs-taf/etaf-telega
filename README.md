# etaf-telega

Emacs 里的 Telegram 客户端：界面用 [ETAF](https://gitea.vhkd.top/geekinney/etaf) 渲染，
账号、会话与 TDLib 数据库仍由 [Telega](https://github.com/zevlg/telega.el) 持有。
不是 Telega 前端的补丁，也不是终端 Telegram 的包装。

## 能力（骨架）

- 左侧对话列表 + 右侧消息流，消息与对话来自 Telega 的 TDLib 事件。
- 底部输入区就在对话界面内原地编辑（`etaf-text-input`）：输入法、选区、撤销都是原生能力。
- 发送在收到回执后才清空草稿；未确认或失败时文本保留。
- 关闭客户端只停止观察，不注销账号、不删除数据。

## 要求

- Emacs 29.1 或更新版本。
- 提供者检出：`etaf`、`etaf-ui`、`ebox`、`ecss`。把它们放在本仓库旁边
  （`../etaf` 等），或放进工作区的上级目录（`../../etaf` 等）。
- 真实使用时还需要 `telega` 包与服务端（`telega-server` + TDLib）。测试与离线
  演示不需要 Telega：`etaf-telega-backend-create-function` 换成假后端即可。

## 安装与运行

把本仓库根、四个提供者的仓库根和 telega 的加载路径加进 `load-path`，然后：

```elisp
(require 'etaf-telega)
M-x etaf-telega-open
```

`etaf-telega-open` 不会显示 Telega 自己的 buffer，也不会重新登录；它复用
Telega 的 server 与账号数据。首次触达真实账号前先看
[Telega 的环境说明](#真实账号的边界)。

## 按键

| 按键 | 作用 |
|---|---|
| `j` / `k`、`n` / `p` | 下一个 / 上一个对话 |
| `RET`、`g` | 重新载入当前对话历史 |
| `i` / `e` | 聚焦底部输入区 |
| 输入区 `RET` | 发送；`Shift+RET` / `C-j` 换行 |
| 输入区 `Esc`、`C-c C-k` | 收起输入区并保留草稿 |
| `q` | 关闭客户端 |

## 开发

```sh
make check     # 结构 + 字节编译 + 验收场景
make test      # 只跑验收场景
make clean
```

验收场景全部走假后端，不连网络、不需要 Telega。真实账号验证必须显式指定
隔离的数据目录，并先停掉其他占用该目录的 TDLib 实例。

界面改动必须真实 GUI 验收；清单与做法见 `pi-skill etaf-app-dev guide verify`。

## 真实账号的边界

- TDLib 同一数据目录只允许一个实例拥有。客户端关闭不等于注销。
- 升级或迁移前停服并备份完整目录（含 WAL），不要拿唯一的生产登录库做实验。
- 本仓库不复制消息数据库；界面所需的状态只在内存里投影。
