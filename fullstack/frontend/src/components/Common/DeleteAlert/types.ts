export enum EntityKind {
  Transaction = "transaction",
  Widget = "widget",
  User = "user",
  Notification = "notification",
}

export type Transaction = {
  kind: EntityKind.Transaction;
  id: number;
};

export type Widget = {
  kind: EntityKind.Widget;
  id: number;
  canvasId: number;
};

export type User = {
  kind: EntityKind.User;
  id: number;
};

export type Notification = {
  kind: EntityKind.Notification;
  id: number;
};

export type EntityType = Transaction | Widget | User | Notification;
