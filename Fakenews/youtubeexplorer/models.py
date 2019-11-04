from django.db import models


class Explorer(models.Model):
    description = models.CharField(max_length=255, blank=True)

# Create your models here.
