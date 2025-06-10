from sqlalchemy import Column, Integer, String, DateTime
from app.models.models import Base


class CronState(Base):
    __tablename__ = "cron_state"
    id = Column(Integer, primary_key=True)
    job_name = Column(String, unique=True, nullable=False)
    last_run = Column(DateTime, nullable=False)
